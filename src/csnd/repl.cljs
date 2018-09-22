(ns csnd.repl
  (:require [csnd.config :as config]
            [csnd.term :refer [term prompt]]
            [csnd.logger :as logger]
            ["csound-api" :as csound]
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
    (.CompileOrc csound Csound input)))

(defn start-repl [state-atom]
  (go-loop [input-stream (input-field state-atom)]
    (let [filename (:filename @state-atom)]
      (swap! state-atom assoc :current-input-stream input-stream)

      (when-not (empty? @logger/logger-buffer)
        (logger/flush-logger-buffer state-atom))
      (let [next-val (chan 1)]

        (-> (.-promise input-stream)
            (.then (fn [input]
                     (when input
                       (when-not (empty? input)
                         (config/update-history! state-atom input)
                         (repl-handler state-atom input))
                       (term "\n"))
                     (go (>! next-val (or input ""))))))
        (let [input (<! next-val)]
          ;; (<! (timeout 20))
          (prompt term filename
                  (get-in @state-atom [:colorset :color3])
                  (get-in @state-atom [:colorset :color2]))
          (recur (input-field state-atom)))))))
