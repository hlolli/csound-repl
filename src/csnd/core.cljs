(ns csnd.core
  (:require [csnd.colors :as colors]
            [csnd.term :refer [term]]
            [csnd.banner :refer [generate-banner]]
            ["../js/mkDir" :default mkDir]
            ["csound-api" :as csound]
            ["cluster" :as cluster]
            ["argparse" :as args]
            ["path" :refer [basename] :as path]
            ["fs" :as fs]
            ["os" :as os]
            [clojure.string :as string]
            [clojure.tools.reader.edn :as edn]
            [cljs.core.async :refer [<! >! put! chan timeout] :as async])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; csound.DisposeOpcodeList(Csound, opcodeList);

(def state (atom {:isCharacter          false
                  :currentLine          ""
                  :current-input-stream nil
                  :x                    0
                  :y                    0}))

(def csound-instance (atom nil))

(.SetDefaultMessageCallback csound (fn []))

(def opcode-symbols (atom []))

(def history (atom []))

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

(defn prompt [term filename]
  (-> term
      (.colorRgbHex.bold
       (:color3 @colors/current-colorset)
       (str "[" filename "] "))
      (.colorRgbHex (:color2 @colors/current-colorset)
                    "csnd> ")
      (.styleReset)))

(def logger-buffer (atom (generate-banner (.GetVersion csound))))

(def logger-queue-size (atom 0))

(def upper-has-logged? (atom false))

(declare logger)


#_(cond
    (and @first-log?
         (not pre-rebase?)
         (< (.-y input-field-location) (.-height term)))
    (and (not pre-rebase?)
         (< (.-y input-field-location) (.-height term)))
    (inc (.-y input-field-location))
    (not pre-rebase?)
    (inc (.-y input-field-location))
    :else
    (.-y input-field-location))

;; (def dbg (atom {:queue [] :called 0}))
;; (swap! dbg update :queue conj (.-y input-field-location))
;; (swap! dbg update :called inc)

(defn rebase [prompt-filename error? final?]
  (let [input-stream         (:current-input-stream @state)
        input-field-location (.getPosition input-stream)
        not-at-bottom?       (< (.-y input-field-location) (.-height term))]
    (js/setTimeout
     #(if-not (number? (.-y input-field-location))
        (rebase prompt-filename error?)
        (do
          (.rebase input-stream
                   (.-x input-field-location)
                   (if not-at-bottom?
                     (inc (.-y input-field-location))                     
                     (.-y input-field-location)))
          (when-not (empty? @logger-buffer)
            (logger prompt-filename
                    (ffirst @logger-buffer)
                    error?)
            (swap! logger-buffer (comp vec rest)))
          (when final?
            (reset! logger-queue-size 0))))
     10)))


(defn logger [prompt-filename msg error?]
  (reset! logger-queue-size (if (>= (count @logger-buffer)
                                    @logger-queue-size)
                              (count @logger-buffer)
                              @logger-queue-size))
  
  (let [input-stream         (:current-input-stream @state)
        cursor-offset        (.getCursorPosition input-stream)
        input-field-location (.getPosition input-stream)
        msg                  (-> msg string/trim
                                 (string/replace "(token \"\n" "(token \"\\n"))
        msg                  (if (re-find #"<<<" msg)
                               (string/replace msg "\n" "??")
                               msg)
        msg                  (string/replace msg "\n" " ")
        ;; msg                  (prn-str msg)
        ]
    (-> term
        (as-> term (term "\n"))
        (.previousLine 1)
        (.eraseLine)
        (.styleReset)
        (.defaultColor msg))
    (let [input-stream (:current-input-stream @state)]
      (when (< 1 (count @logger-buffer))
        (rebase prompt-filename error? false))
      (when (>= 1 (count @logger-buffer))
        (rebase prompt-filename error? true)
        (js/setTimeout #(do (-> term
                                (.column 0)
                                (prompt prompt-filename))) 10)))))

#_(defn logger [prompt-filename msg error?]
    (let [terminal-width (.-width term)
          chopped-msg    (string/split msg "\n")
          #_             (map (partial apply str)
                              (partition-all terminal-width msg))]
      (run! #(logger* prompt-filename % error?)
            chopped-msg)))

(defn exit-gracefully [exit-code]
  (when-let [Csound @csound-instance]
    (.Stop csound Csound)
    (.Destroy csound Csound))
  (.processExit term exit-code))

;; (empty? (string/trim msg)) nil
;; (= " <<<\n" msg)
;; (let [_       (swap! logger-buffer conj msg)
;;       vek     (reverse @logger-buffer)
;; out-str 
;; out-str (str "  <<<" out-str ">>>")]
;;   (reset! logger-buffer [])
;;   (logger prompt-filename out-str error?))
;; (or (not (empty? @logger-buffer))
;;     (re-find #"line.*\n>>>" msg))
;; (swap! logger-buffer conj msg)


(defn fix-token-error-log [sek]
  (str "<<< "
       (-> (apply str (interpose " " sek))
           (string/replace "\n" " ")
           string/trim
           (string/replace ">>> " "")
           (string/replace " <<<" ""))
       ">>>"))

(defn repair-logs [queue-atom]
  (loop [[[msg error?] & msgs] @queue-atom
         out                   []
         token-error           []
         end-of-score          false
         overall-samps         false]
    (cond
      (nil? msg) out
      (re-find #"line.*\n>>>" msg)
      (recur msgs out (conj token-error msg) end-of-score overall-samps)
      (= " <<<\n" msg)
      (recur
       msgs
       (conj out [(fix-token-error-log (conj token-error msg)) error?])
       [] end-of-score overall-samps)
      (not (empty? token-error))
      (recur msgs out (conj token-error msg) end-of-score overall-samps)
      (re-find #"end of score\." msg)
      (recur msgs out token-error msg overall-samps)
      (string? end-of-score)
      (recur msgs (conj out [(str (string/replace end-of-score "\t" "") " " msg) error?])
             token-error false overall-samps)
      (re-find #"overall samples out of range" msg)
      (recur msgs out token-error end-of-score msg)
      (string? overall-samps)
      (recur msgs (conj out [(str overall-samps " " msg) error?])
             token-error end-of-score false)
      :else
      (recur msgs (conj out [msg error?]) token-error end-of-score overall-samps))))

(defn start-csound [filepath config]
  (let [Csound        (.Create csound "-m0")
        filename      (path/basename filepath)
        _             (.SetDefaultMessageCallback
                       csound (fn [code msg]
                                (when-not (empty? (string/trim msg))
                                  (when (empty? @logger-buffer)
                                    (js/setTimeout
                                     #(do (reset! logger-buffer (repair-logs logger-buffer))
                                          (logger filename
                                                  (ffirst @logger-buffer)
                                                  (second (first @logger-buffer)))
                                          (swap! logger-buffer (comp vec rest)) ) 10))
                                  (swap! logger-buffer conj
                                         [msg (not= 0 code)]))))
        file-contents (fs/readFileSync filepath)]
    (.SetOption csound Csound "--output=dac")
    (case (path/extname filepath)
      ".orc" (.CompileOrc csound Csound file-contents)
      (do (logger filename
                  (str (path/basename filepath)
                       " is not a valid csound file.")
                  true)
          (exit-gracefully 1)))
    ;; (js/setTimeout #(.Message csound Csound "hello 100000000001 100000000000000001 999") 1000)
    ;; (js/setTimeout #(.Message csound Csound "hello 2") 2000)
    
    (if (= (.-SUCCESS csound) (.Start csound Csound))
      (.PerformAsync csound Csound #(.Destroy csound Csound))
      (logger filename "Csound couldn't be started" true))
    (reset! csound-instance Csound)))


(defn input-field []
  (.inputField term
               #js {
                    :autoComplete
                    (fn [input-string]
                      (cond
                        (> 2 (count input-string)) ""
                        (re-find #"[^a-zA-Z0-9]" input-string)
                        ""
                        :else
                        (or (first
                             (filter #(re-find (re-pattern input-string) %)
                                     @opcode-symbols)) "")))
                    :echo             true
                    :cancelable       true
                    :autoCompleteHint true
                    :autoCompleteMenu true
                    :history          (clj->js @history)
                    :keyBindings
                    #js {"CTRL_C"    "cancel"
                         "ESCAPE"    "cancel"
                         "TAB"       "autoComplete"
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
                         }
                    }
               #_(fn [error input]
                   (js/console.log "\nYour input is: " input "error is: " error)
                   ;;(.redraw this)
                   
                   ;; (re-prompt)
                   #_(.exit js/process 0))))

(defn set-global-keys []
  (.on term "key"
       (fn [name matches data]
         (let [input-stream (:current-input-stream @state)
               cur-line     (and input-stream (.getInput input-stream))]
           (case name
             ("ESCAPE" "CTRL_C")
             (when input-stream
               (.deleteLine term 1))
             "CTRL_D"
             (when (empty? cur-line)
               (when input-stream
                 (let [input-field-location (.getPosition input-stream)
                       bottom?              (>= (.-y input-field-location) (.-height term))]
                   (.abort input-stream)
                   (when bottom?
                     (-> term (.previousLine 1) (.deleteLine 1)))))
               ;; (.deleteLine term 0)
               (exit-gracefully 0))
             nil))
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
  (let [base-filename (path/basename file)]
    (fs/watch file #js {:persistent false}
              (fn [event-type filename]
                (when (= "change" event-type)
                  (logger base-filename
                          (str "--> Changes detected in "
                               base-filename
                               " recompiling...")
                          false))))))

(defn main [& _args]
  (let [args        (.parseArgs argument-parser)
        filename    (basename (.-file args))
        abs-path    (path/normalize
                     (path/resolve
                      (.-file args)))
        cfg-dir     (path/join (os/homedir) ".csnd")
        history-loc (path/join cfg-dir "history.edn")]
    (mkDir cfg-dir)
    (when (fs/existsSync history-loc)
      (reset! history
              (try (into [] (edn/read-string
                             (.toString (fs/readFileSync history-loc))))
                   (catch js/Error e []))))
    (when-not (fs/existsSync (.-file args))
      (colors/error-color
       (str "File " abs-path " doesn't exist!"))
      (exit-gracefully 1))
    ;; (js/console.log args)
    (.windowTitle term "Csound REPL")
    (set-global-keys)
    (reset! opcode-symbols (get-opcode-symbols))
    (prompt term filename)

    (start-csound abs-path {})
    (file-watcher abs-path)
    (go-loop [input-stream (input-field)]
      (swap! state assoc :current-input-stream input-stream)

      #_(when @first-log?
          (js/console.log input-stream)
          (.pause input-stream))
      (when-not (empty? @logger-buffer)
        (let [[msg error?] (first @logger-buffer)]
          (logger filename msg error?)
          (swap! logger-buffer (comp vec rest))))
      (let [next-val (chan 1)]
        ;; (set-global-keys term)
        (-> (.-promise input-stream)
            (.then (fn [input]
                     (when input
                       (when-not (empty? input)
                         (let [new-history (vec (conj (take 49 @history) input))]
                           (reset! history new-history)
                           (fs/writeFileSync history-loc new-history))
                         (when-let [Csound @csound-instance]
                           ;; (.ReadScore csound Csound input)
                           (.CompileOrc csound Csound input)
                           ))
                       (term "\n"))
                     (go (>! next-val (or input ""))))))
        (let [input (<! next-val)]
          ;; (<! (timeout 20))
          (prompt term filename)
          (recur (input-field)))))))

