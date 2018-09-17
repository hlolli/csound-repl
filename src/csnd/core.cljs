(ns csnd.core
  (:require [csnd.colors :as colors]
            [csnd.repl :as repl]
            [csnd.term :refer [term prompt exit-gracefully]]
            [csnd.config :as config]
            [csnd.logger :refer [logger logger-buffer unsafe-log]]
            [csnd.global-keybindings :as global-keybindings]
            ["csound-api" :as csound]
            ["argparse" :as args]
            ["path" :refer [basename] :as path]
            ["fs" :as fs]
            [clojure.string :as string]))

;; csound.DisposeOpcodeList(Csound, opcodeList);

(def state (atom {:current-input-stream nil
                  :csound-instance      nil
                  :opcode-symbols       []
                  :history              []
                  :filename             ""
                  :filepath             ""
                  :colorset             {}}))

;; (def csound-instance (atom nil))

(.SetDefaultMessageCallback csound (fn []))

;; (def opcode-symbols (atom []))

;; (def history-atom (atom []))

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
      (->> (js->clj sym-arr)
           (remove empty?)
           (remove #{"sr" "poscil"})))))

#_(defn logger [prompt-filename msg error?]
    (let [terminal-width (.-width term)
          chopped-msg    (string/split msg "\n")
          #_             (map (partial apply str)
                              (partition-all terminal-width msg))]
      (run! #(logger* prompt-filename % error?)
            chopped-msg)))


(defn fix-token-error-log [sek]
  (str "<<< "
       (-> (apply str (interpose " " sek))
           (string/replace "\n" "")
           string/trim
           (string/replace ">>> " "")
           (string/replace " <<<" "")
           (string/replace " " "")
           (string/replace ":" ": ")
           (string/replace #"(line)([0-9])" "$1 $2"))
       " >>>"))

(defn repair-logs [queue-atom]
  (loop [[[msg error?] & msgs] @queue-atom
         out                   []
         token-error           []
         end-of-score          false
         overall-samps         false
         plain-error           false]
    (cond
      (nil? msg) out
      (re-find #"line.*\n>>>" msg)
      (recur msgs out (conj token-error msg) end-of-score overall-samps plain-error)
      (= " <<<\n" msg)
      (recur
       msgs
       (conj out [(fix-token-error-log (conj token-error msg)) error?])
       [] end-of-score overall-samps plain-error)
      (not (empty? token-error))
      (recur msgs out (conj token-error msg) end-of-score overall-samps plain-error)
      (re-find #"end of score\." msg)
      (recur msgs out token-error msg overall-samps plain-error)
      (string? end-of-score)
      (recur msgs (conj out [(str (string/replace end-of-score "\t" "") " " msg) error?])
             token-error false overall-samps plain-error)
      (re-find #"overall samples out of range" msg)
      (recur msgs out token-error end-of-score msg plain-error)
      (string? overall-samps)
      (recur msgs (conj out [(str overall-samps " " msg) error?])
             token-error end-of-score false plain-error)
      (= "error:  " msg)
      (recur msgs out token-error end-of-score overall-samps "error: ")
      (string? plain-error)
      (recur msgs (conj out [(str plain-error " " msg) error?])
             token-error end-of-score overall-samps false)
      (re-find #"rtevent:" msg)
      (recur (rest msgs)
             (conj out [(str (string/replace msg "\t" "") " "
                             (string/trim (ffirst msgs))) error?])
             token-error end-of-score overall-samps plain-error)
      :else
      (recur msgs (conj out [msg error?]) token-error end-of-score overall-samps plain-error))))

(defn start-csound [filepath config]
  (let [Csound        (.Create csound "-m0")
        filename      (path/basename filepath)
        _             (.SetDefaultMessageCallback
                       csound (fn [code msg]
                                (when-not (empty? (string/trim msg))
                                  (when (empty? @logger-buffer)
                                    (js/setTimeout
                                     #(do (reset! logger-buffer (repair-logs logger-buffer))
                                          (logger state
                                                  filename
                                                  (ffirst @logger-buffer)
                                                  (second (first @logger-buffer)))
                                          (swap! logger-buffer (comp vec rest)) ) 10))
                                  (swap! logger-buffer conj
                                         [msg (not= 0 code)]))))
        file-contents (fs/readFileSync filepath)]
    (.SetOption csound Csound "--output=dac")
    (case (path/extname filepath)
      ".orc" (.CompileOrc csound Csound file-contents)
      (do (logger state
                  filename
                  (str (path/basename filepath)
                       " is not a valid csound file.")
                  true)
          (exit-gracefully state 1)))
    ;; (js/setTimeout #(.Message csound Csound "hello 100000000001 100000000000000001 999") 1000)
    ;; (js/setTimeout #(.Message csound Csound "hello 2") 2000)
    
    (if (= (.-SUCCESS csound) (.Start csound Csound))
      (.PerformAsync csound Csound #(.Destroy csound Csound))
      (logger state filename "Csound couldn't be started" true))
    (swap! state assoc :csound-instance Csound)))


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
                  (logger state
                          base-filename
                          (str "--> Changes detected in "
                               base-filename
                               " recompiling...")
                          false))))))


(defn main [& _args]
  (let [args     (.parseArgs argument-parser)
        filename (basename (.-file args))
        abs-path (path/normalize
                  (path/resolve
                   (.-file args)))]
    (swap! state assoc :filename filename :filepath abs-path)
    (config/load-configuration! state)
    (config/load-history! state)
    (when-not (fs/existsSync (.-file args))
      (colors/error-color
       (str "File " abs-path " doesn't exist!"))
      (exit-gracefully state 1))
    ;; (js/console.log args)
    (.windowTitle term "Csnd - Csound REPL")
    (global-keybindings/set-global-keys filename state)
    (swap! state update :opcode-symbols into (get-opcode-symbols))
    (start-csound abs-path {})
    (file-watcher abs-path)
    (repl/start-repl state)
    (prompt term filename
            (get-in @state [:colorset :color3])
            (get-in @state [:colorset :color2]))
    ))

