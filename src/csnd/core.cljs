(ns csnd.core
  (:require [csnd.colors :as colors]
            [csnd.term :refer [term]]
            ["csound-api" :as csound]
            ["cluster" :as cluster]
            ["argparse" :as args]
            ["path" :refer [basename] :as path]
            ["fs" :as fs]
            [clojure.string :as string]
            [cljs.core.async :refer [<! >! put! chan timeout] :as async])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; csound.DisposeOpcodeList(Csound, opcodeList);

(def state (atom {:isCharacter          false
                  :currentLine          ""
                  :current-input-stream nil}))

(def csound-instance (atom nil))

(.SetDefaultMessageCallback csound (fn []))

(def opcode-symbols (atom []))

(defn get-opcode-symbols []
  (let [js-arr #js []
        Csound (.Create csound "-m0")]
    (.SetMessageCallback csound Csound (fn []))
    (doto Csound
      ((.-NewOpcodeList csound) js-arr))
    ((.-Destroy csound) Csound)
    (let [sym-arr (amap js-arr indx ret
                        (-> (js/Object.values
                             (aget js-arr indx))
                            (aget 2)))]
      (remove empty? (js->clj sym-arr)))))

(defn prompt [filename]
  (if filename
    (str ((.colorRgbHex
           (.bold term)
           (:color3 @colors/current-colorset))
          (str "[" filename "] "))
         ((.colorRgbHex
           (.bold term)
           (:color2 @colors/current-colorset))
          "csnd> ")
         (.styleReset term))
    ((.colorRgbHex
      (.bold term)
      (:color2 @colors/current-colorset))
     ;;"🎵> "
     "csnd> "
     )))

(def logger-buffer (atom []))

(defn logger [prompt-filename msg error?]
  (let [cursor-offset
        (if-let [input-stream (:current-input-stream @state)]
          (.getCursorPosition input-stream) 0)
        input-field-location
        (if-let [input-stream (:current-input-stream @state)]
          (.getPosition input-stream) 0)]
    (when-let [input-stream (:current-input-stream @state)]
      (.hide input-stream)
      (.pause input-stream)
      (.scrollUp term 1)
      (.previousLine term 1)
      (.eraseLine term))
    (if error?
      (colors/csound-log-color-error msg)
      (colors/csound-log-color msg))
    (when-let [input-stream (:current-input-stream @state)]
      (.rebase input-stream
               0
               (inc (.-y input-field-location)))
      (.eraseLine term)
      (.column term 0)
      (prompt prompt-filename)
      (.rebase  input-stream
                (.-x input-field-location)
                (.-y input-field-location))
      (.resume input-stream)
      (.show input-stream)
      ;; (.redraw input-stream)
      ))
  ;;(js/console.log msg)
  ;; (.scrollUp term 1)
  )



(defn start-csound [filepath config]
  (let [Csound        (.Create csound "-m0")
        filename      (path/basename filepath)
        _             (.SetDefaultMessageCallback
                       csound (fn [code msg]
                                (if-not (:current-input-stream @state)
                                  (swap! logger-buffer conj [msg (not= 0 code)])
                                  (logger filename msg (not= 0 code)))))
        file-contents (fs/readFileSync filepath)]
    (.SetOption csound Csound "--output=dac")
    (case (path/extname filepath)
      ".orc" (.CompileOrc csound Csound file-contents)
      (do (colors/error-color
           (str (path/basename filepath)
                " is not a valid csound file."))
          (.processExit term 0)))
    (js/setTimeout #(.Message csound Csound "HELLO!") 2000)
    (js/setTimeout #(.Message csound Csound "HELLO!") 5000)
    (reset! csound-instance Csound)))


(defn input-field []
  (.inputField term
               #js {
                    :autoComplete
                    (fn [input-string]
                      (cond
                        (> 2 (count input-string)) ""
                        :else
                        (or (first
                             (filter #(re-find (re-pattern input-string) %)
                                     @opcode-symbols)) "")))
                    :echo             true
                    :cancelable       true
                    :autoCompleteHint true
                    :autoCompleteMenu true
                    :keyBindings
                    #js {"CTRL_C"    "cancel"
                         "ESCAPE"    "cancel"
                         "TAB"       "autoComplete"
                         ;; "SHIFT_TAB" "cyclePrevious"
                         "ENTER"     "submit"
                         "KP_ENTER"  "submit"
                         "BACKSPACE" "backDelete"
                         "DELETE"    "delete"
                         "CTRL_U"    "deleteAllBefore"
                         "CTRL_K"    "deleteAllAfter"
                         "LEFT"      "backward"
                         "RIGHT"     "forward"
                         "DOWN"      "historyNext"
                         "UP"        "historyPrevious"
                         "HOME"      "first"
                         "END"       "last"
                         "CTRL_X"    (fn [] (prn "TEST!"))}
                    }
               #_(fn [error input]
                   (js/console.log "\nYour input is: " input "error is: " error)
                   ;;(.redraw this)
                   
                   ;; (re-prompt)
                   #_(.exit js/process 0))))

(defn set-global-keys []
  (.on term "key"
       (fn [name matches data]
         (case name
           ("ESCAPE" "CTRL_C")
           (do (.deleteLine term 1)
               (if-let [input-stream (:current-input-stream @state)]
                 (when (empty? (.getInput input-stream))
                   (.scrollDown term 1)
                   (.processExit term 0))
                 (when (empty? (:currentLine @state))
                   (.scrollDown term 1)
                   (.processExit term 0))))
           nil)
         (-> (.getCursorLocation term)
             (.then (fn [location]
                      (when location
                        (let [x        (if location (.-x location) 0)
                              y        (if location (.-y location) 0)
                              cur-line (:currentLine @state)]
                          (swap! state assoc :currentLine
                                 (case name
                                   "BACKSPACE"
                                   (if (empty? cur-line)
                                     ""
                                     (subs cur-line 0 (dec (count cur-line))))
                                   ("ESCAPE" "CTRL_C") ""
                                   ;; ELSE
                                   (if (.-isCharacter data)
                                     (str cur-line name)
                                     cur-line))))))))
         
         #_(prn "name" name
                "matches" matches
                "data" (.toString (.-code data))))))

(def argument-parser
  (new (.-ArgumentParser args)
       #js {:version     "1.0.0-alpha"
            :addHelp     "true"
            :description "Csound terminal REPL"}))

(defn add-argument [argument config]
  (.addArgument argument-parser
                argument config))

(add-argument "file"
              #js {:help "A csound file (csd/orc/sco)"
                   :type "string"})

(add-argument #js ["-k" "--ksmps"]
              #js {:help "A global ksmps value"
                   :type "int"})

(defn file-watcher [file]
  (fs/watch file #js {:persistent false}
            (fn [event-type filename]
              (when (= "change" event-type)
                ))))

(defn main [& _args]
  (let [args     (.parseArgs argument-parser)
        filename (basename (.-file args))
        abs-path (path/normalize
                  (path/resolve
                   (.-file args)))]
    (when-not (fs/existsSync (.-file args))
      (colors/error-color
       (str "File " abs-path " doesn't exist!"))
      (.processExit term 1))
    ;; (js/console.log args)
    (.windowTitle term "Csound REPL")
    (set-global-keys)
    (reset! opcode-symbols (get-opcode-symbols))
    (prompt filename)
    (start-csound abs-path {})
    (file-watcher abs-path)
    (go-loop [input-stream (input-field)]
      (swap! state assoc :current-input-stream input-stream)
      (when-not (empty? @logger-buffer)
        (run! (fn [[msg error?]]
                (js/setTimeout #(logger filename msg error?) 10)) @logger-buffer)
        (reset! logger-buffer []))
      (let [next-val (chan 1)]
        ;; (set-global-keys term)
        (-> (.-promise input-stream)
            (.then (fn [input]
                     (when input
                       (js/console.log "\n")
                       (.scrollDown term 1))
                     (go (>! next-val (or input ""))))))
        (let [input (<! next-val)]
          (prompt filename)
          (recur (input-field))))))

  ;;(input-field)
  )

