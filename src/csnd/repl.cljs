(ns csnd.repl
  (:require [csnd.config :as config]
            [csnd.term :refer [term prompt multiline-prompt]]
            [csnd.logger :as logger]
            ["csound-api" :as csound]
            [clojure.string :as string]
            [cljs.core.async :refer [<! >! put! chan timeout] :as async])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(def key-bindings
  #js {"CTRL_C"     "cancel"
       "ESCAPE"     "cancel"
       "TAB"        "autoComplete"
       "ENTER"      "submit"
       "KP_ENTER"   "submit"
       "BACKSPACE"  "backDelete"
       "DELETE"     "delete"
       "CTRL_U"     "deleteAllBefore"
       "CTRL_K"     "deleteAllAfter"
       "LEFT"       "backward"
       "RIGHT"      "forward"
       "DOWN"       "historyNext"
       "UP"         "historyPrevious"
       "HOME"       "startOfInput"
       "END"        "endOfInput"
       "CTRL_A"     "startOfInput"
       "CTRL_E"     "endOfInput"
       "CTRL_LEFT"  "previousWord"
       "CTRL_RIGHT" "nextWord"
       "ALT_D"      "deleteNextWord"
       "CTRL_W"     "deletePreviousWord"})

(defn token-hook [state-atom]
  ;; set! config.style
  ;; (set! (.-tokenRegExp config) (js/RegExp. "\\S+" "g"))

  (fn [token isEndOfInput previousTokens term config]
    ;; (prn  (string/join "\b" [token isEndOfInput previousTokens term config]))
    (cond
      ;; Comments
      (and (or (re-find #";|/\*" token)
               (re-find #";|/\*" (apply str previousTokens)))
           (not (re-find #"\*/" (apply str previousTokens))))
      (set! (.-style config) (.-dim (.italic (.styleReset  term))))
      ;; Operators
      (re-matches #"\+|\-|\*|/|=|%|while|od" token)
      (set! (.-style config) (.styleReset term))
      ;; P-fields
      (re-matches #"p[0-9]+" token)
      (set! (.-style config) (.-bold term))
      ;; Constants
      (re-matches #"sr|ksmps|kr|0dbfs|nchnls" token)
      (set! (.-style config) (.-bold (.colorRgbHex term (get-in @state-atom [:colorset :color3]))))
      ;; Opcodes
      (some #(= token %) (:opcode-symbols @state-atom))
      (set! (.-style config) (.-bold (.colorRgbHex term (get-in @state-atom [:colorset :color2]))))
      ;; Audio signal
      (re-matches #"^(a|ga).+" token)
      (set! (.-style config) (.-bold (.colorRgbHex term (get-in @state-atom [:colorset :color4]))))
      ;; All other varable types
      (re-matches #"^(k|gk|i|gi|f|gf).+" token)
      (set! (.-style config) (.-bold (.colorRgbHex term (get-in @state-atom [:colorset :color5]))))
      :else (.styleReset term))))

(defn auto-complete [state-atom]
  (fn [input-string]
    (cond
      (> 2 (count input-string)) ""
      (re-find #"[^a-zA-Z0-9]" input-string)
      ""
      :else
      (or (first
           (filter #(re-find (re-pattern input-string) %)
                   (:opcode-symbols @state-atom))) ""))))

(defn input-field [state-atom]
  (let [token-hook    (token-hook state-atom)
        auto-complete (auto-complete state-atom)]
    (.inputField term
                 #js {:autoComplete     auto-complete
                      :echo             true
                      :cancelable       true
                      :autoCompleteHint true
                      :autoCompleteMenu true
                      :history          (clj->js (:history @state-atom))
                      :tokenHook        token-hook
                      :keyBindings      key-bindings
                      })))

(defn repl-handler [state-atom input]
  ;; (let [input (string/trim input)])
  (when-let [Csound (:csound-instance @state-atom)]
    ;; (prn (prn-str input) "\n" (prn-str input))
    (.CompileOrc csound Csound input)))

(def multiline-forms
  {"opcode" "endop"
   "instr"  "endin"})

(defn find-multiline-token-opener [line]
  (let [hash-of-openers (into #{} (keys multiline-forms))]
    (loop [[token & line] (string/split (str line) " ")
           opener         nil]
      (if (nil? token)
        opener
        (recur line
               (if-not opener
                 #_(prn (hash-of-openers token) hash-of-openers token (str line))
                 (hash-of-openers token)
                 (if (= token (get multiline-forms opener)) nil opener)))))))


(def paste-buffer (atom ""))

(defn pre-paste-rebase [state-atom x-offset]
  (let [input-stream         (:current-input-stream @state-atom)
        input-field-location (if input-stream
                               (.getPosition input-stream)
                               #js {:y 0})
        not-at-bottom?       (< (.-y input-field-location) (.-height term))]
    (when input-stream
      (.rebase input-stream
               ;; (.-x input-field-location)
               x-offset
               (if not-at-bottom?
                 (inc (.-y input-field-location))                     
                 (.-y input-field-location))))))


(defn generate-spaces [cnt]
  (apply str (take cnt (cycle [" "]))))

(defn set-head-value [state-atom chunk]
  (let [elem         (.-inputs (:current-input-stream @state-atom))
        last-elem    (if elem (aget (.slice elem -1) 0) nil)
        single-chars (clj->js (vec (remove empty? (string/split chunk ""))))]
    (set! (.-inputIndex (:current-input-stream @state-atom))
          (if (empty? last-elem)
            (dec (dec (.-length (.-inputs (:current-input-stream @state-atom)))))
            (dec (.-length (.-inputs (:current-input-stream @state-atom))))))
    (when (and elem last-elem)
      (do (when (empty? last-elem)
            (.splice (.-inputs (:current-input-stream @state-atom)) -1 1)))
      (.concat (.-inputs (:current-input-stream @state-atom))
               (clj->js [single-chars])))))

(defn start-repl [state-atom]
  (let [paste-buffer-throttle (atom nil)
        input-listener
        (.on term "key"
             (fn [name matches data]
               (when (or (.-isCharacter data)
                         (< -1 (.indexOf matches "ENTER")))
                 (swap! paste-buffer str (if (.-isCharacter data) name "\n")))
               (when-let [timeout-throttle @paste-buffer-throttle]
                 (js/clearTimeout timeout-throttle))
               (reset! paste-buffer-throttle
                       (js/setTimeout #(reset! paste-buffer "") 10))
               #_(js/setTimeout
                  #(swap! logger/logger-buffer conj [(str " \nk>  " (prn-str name) " "
                                                          (prn-str matches) " "
                                                          (prn-str (js->clj data))
                                                          " <k\n")
                                                     false]) 1000)))]
    (go-loop [input-stream    (input-field state-atom)
              multiline-form  nil
              multiline-data  []]
      (let [filename (:filename @state-atom)]
        ;; (js/console.log (str "METHODS \n" (string/replace (str (js/Object.keys input-stream)) "," "\n") "\n \n "))
        (swap! state-atom assoc :current-input-stream input-stream)
        (when-not (empty? @logger/logger-buffer)
          (logger/flush-logger-buffer state-atom))
        (let [next-val (chan 1)]
          (-> (.-promise input-stream)
              (.then (fn [input]
                       (if multiline-form (.disableHistory (:current-input-stream @state-atom)))
                       (go (>! next-val (or input ""))))))
          
          (let [input (<! next-val)]
            (<! (timeout 1))
            (let [last-form          multiline-form
                  cur-multiline-form (if-let [token multiline-form]
                                       (if (->> ;; just look at all data possible
                                            (reduce (fn [i v]
                                                      (conj i (some #(= (get multiline-forms token) %)
                                                                    (string/split (str v) " "))))
                                                    []
                                                    (into multiline-data
                                                          (conj
                                                           (vec (string/split @paste-buffer "\n"))
                                                           (str input))))
                                            (some true?))
                                         nil multiline-form)
                                       (find-multiline-token-opener input))
                  multiline-data     (if multiline-form
                                       (conj multiline-data (str input))
                                       (if (and last-form (not multiline-data))
                                         []
                                         multiline-data))
                  paste-chunks       (if (< 1 (count (filter #(= "\n" %) (string/trim @paste-buffer))))
                                       (let [paste-chunks (string/split @paste-buffer "\n")]
                                         paste-chunks)
                                       [])
                  multiline-data     (into multiline-data paste-chunks)
                  multiline-form     cur-multiline-form]

              (when-not (empty? paste-chunks)
                ;; (prn "\n\n\n WE HAVE PASE CHUNKS \n\n\n")
                ;; (set! (.-pause (:current-input-stream @state-atom)) true)
                (pre-paste-rebase state-atom (+ 9 (count filename)))
                (term (generate-spaces (* 2 (count (string/trim (first paste-chunks))))))
                ;; (.eraseLineAfter term)
                (term "\n")
                (loop [[chunk & chunks] (vec (rest paste-chunks))]
                  (when-not (nil? chunk)
                    (let [chunk (string/trim chunk)]
                      (set-head-value state-atom chunk)
                      (pre-paste-rebase state-atom 0)
                      (-> term (.eraseLine) (.column 0))
                      (multiline-prompt term filename
                                        (get-in @state-atom [:colorset :color3])
                                        (get-in @state-atom [:colorset :color2])
                                        (if last-form 1 0))
                      (.insert term (count chunk))
                      ;; (.show (:current-input-stream @state-atom))
                      ;; (term chunk)
                      (.writeTokens (:current-input-stream @state-atom) chunk)
                      (set-head-value state-atom chunk)
                      ;; (.redraw (:current-input-stream @state-atom) 0 false)
                      (term "\n")
                      ;; (when-not (nil? chunks) (term "\n"))
                      (recur chunks)))))

              (when (empty? paste-chunks)
                (term "\n"))
              
              ;; Always clear this
              (reset! paste-buffer "")
              

              ;; (term multiline-form)
              (if multiline-form ;; (find-multiline-token-opener (string/join " " multiline-data))
                (do (.disableHistory (:current-input-stream @state-atom))
                    (multiline-prompt term filename
                                      (get-in @state-atom [:colorset :color3])
                                      (get-in @state-atom [:colorset :color2])
                                      (if last-form 1 0)))
                (let [input-bundle (if (empty? multiline-data)
                                     (str (or input ""))
                                     (string/join "\n" multiline-data))]
                  #_(js/setTimeout
                     #(swap! logger/logger-buffer conj [(str " \n>  " (prn-str input-bundle)
                                                             " < \n")
                                                        false]) 1000)
                  (when (not (empty? input-bundle))
                    ;; (prn "\nUPDATE HISTORY!!\n")
                    (config/update-history! state-atom input-bundle)
                    ;; (.redraw (:current-input-stream @state-atom) 4 true)
                    (repl-handler state-atom input-bundle))
                  ;; (<! (timeout 1000))
                  (prompt term filename
                          (get-in @state-atom [:colorset :color3])
                          (get-in @state-atom [:colorset :color2]))
                  (.disableHistory (:current-input-stream @state-atom))))
              (.abort (:current-input-stream @state-atom))
              (recur (input-field state-atom)
                     multiline-form
                     (if-not multiline-form
                       []
                       multiline-data)))))))))

#_(

   instr 2
   a1      oscil   0.1, 440, -1
   out     a1   
   endin
   
   )
