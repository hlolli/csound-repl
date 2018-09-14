(ns csnd.core
  (:require [csnd.colors :as colors]
            ["csound-api" :as csound]
            ["cluster" :as cluster]
            ["argparse" :as args]
            ["terminal-kit" :as terminal]
            ["path" :refer [basename] :as path]
            ["fs" :as fs]
            [clojure.string :as string]
            [cljs.core.async :refer [<! >! put! chan timeout] :as async])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; csound.DisposeOpcodeList(Csound, opcodeList);

(def state (atom {:isCharacter false
                  :currentLine ""}))

(def csound-instance (atom nil))

(def current-colorset (atom colors/cyberpunk-neon))

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


(defn start-csound [filepath config term]
  (let [instance (.Create csound "-m0")
        _        (.SetDefaultMessageCallback
                  csound (fn [code msg]
                           (.previousLine term 1)
                           (if (= 0 code)
                             ((.colorRgbHex term
                                            (:color1 @current-colorset))
                              (str "\n MSG: " msg "\n"))
                             ((.colorRgbHex
                               term
                               "#FF0000")
                              msg))
                           ;;(js/console.log msg)
                           ;; (.scrollUp term 1)
                           ))]
    (.SetOption csound instance "--output=dac")
    ;; (.SetOption csound instance "--output=dac")
    (.Message csound instance "HELLO WORLD!")
    (reset! csound-instance instance)))

(def term (.-terminal terminal))

(defn prompt [filename]
  (if filename
    (str ((.colorRgbHex
           (.bold term)
           (:color3 @current-colorset))
          (str "[" filename "] "))
         ((.colorRgbHex
           (.bold term)
           (:color2 @current-colorset))
          "csnd> "))
    ((.colorRgbHex
      (.bold term)
      (:color2 @current-colorset))
     ;;"🎵> "
     "csnd> "
     )))

(defn error-color [string]
  ((.colorRgbHex
    (.bold term)
    "#FF0000")
   string))

(defn input-field []
  (.-promise
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
                    #_(.exit js/process 0)))))

(defn set-global-keys [term]
  (.on term "key"
       (fn [name matches data]
         (case name
           ("ESCAPE" "CTRL_C")
           (do (.deleteLine term 1)
               (when (empty? (:currentLine @state))
                 (.processExit term 0)))
           ;; ("ENTER" "KP_ENTER")
           ;; (.scrollDown term 1)
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
      (error-color
       (str "File " abs-path " doesn't exist!"))
      (.processExit term 1))
    ;; (js/console.log args)
    (.windowTitle term "Csound REPL")
    (set-global-keys term)
    (reset! opcode-symbols (get-opcode-symbols))
    (prompt filename)
    (start-csound abs-path {} term)
    (file-watcher abs-path)
    (go-loop [promise (input-field)]
      (let [next-val (chan 1)]
        ;; (set-global-keys term)
        (-> promise
            (.then (fn [input]
                     (when input (js/console.log "\n"))
                     (go (>! next-val (or input ""))))))
        (let [input (<! next-val)]
          (prompt filename)
          (recur (input-field))))))

  ;;(input-field)
  )

