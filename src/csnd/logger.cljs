(ns csnd.logger
  (:require [csnd.term :refer [term prompt]]
            [clojure.string :as string]
            [csnd.banner :refer [generate-banner]]
            ["csound-api" :as csound]))

(def logger-buffer (atom (generate-banner (.GetVersion csound))))

(declare logger)

(defn rebase [state-atom prompt-filename error? final?]
  (let [input-stream         (:current-input-stream @state-atom)
        input-field-location (if input-stream
                               (.getPosition input-stream)
                               #js {:y 0})
        not-at-bottom?       (< (.-y input-field-location) (.-height term))]
    (when input-stream
      (js/setTimeout
       #(if-not (number? (.-y input-field-location))
          (rebase state-atom prompt-filename error? final?)
          (do
            (.rebase input-stream
                     (.-x input-field-location)
                     (if not-at-bottom?
                       (inc (.-y input-field-location))                     
                       (.-y input-field-location)))
            (when-not (empty? @logger-buffer)
              (logger state-atom
                      prompt-filename
                      (ffirst @logger-buffer)
                      (-> @logger-buffer first second))
              (swap! logger-buffer (comp vec rest)))))
       10))))


(defn logger [state-atom prompt-filename msg error?]
  (let [input-stream         (:current-input-stream @state-atom)
        ;; cursor-offset        (if input-stream
        ;;                        (.getCursorPosition input-stream) 0)
        ;; msg                  (prn-str msg)
        input-field-location (if input-stream (.getPosition input-stream) 0)
        msg                  (str msg)
        msg                  (-> msg (string/replace "(token \"\n" "(token \"\\n"))
        msg                  (string/replace msg "\n" " ")
        msg                  (if (re-matches #".*notrim.*" (str error?)) msg (string/trim msg))
        msg                  (if (re-find #"tab" (str error?)) (str " " msg) msg)
        ;; msg                  (prn-str msg)
        ]
    (-> term
        (as-> term (term "\n"))
        (.previousLine 1)
        (.eraseLine)
        (.styleReset)
        (as-> term
            (cond
              (or (re-matches #".*warn.*" (str error?))
                  (re-matches #"WARNING:.*" msg))
              (.colorRgbHex term (get-in @state-atom [:colorset :color5]))
              (re-matches #".*banner.*" (str error?))
              (.bold (.italic (.colorRgbHex term (get-in @state-atom [:colorset :color5]))))
              (re-find #"filled1" (str error?))
              (.bold (.inverse (.colorRgbHex term (get-in @state-atom [:colorset :color1]))))
              :else
              (.defaultColor term)))
        (as-> term (term msg))
        (.styleReset))
    (let [input-stream (:current-input-stream @state-atom)]
      (when (< 1 (count @logger-buffer))
        (rebase state-atom prompt-filename error? false))
      (when (>= 1 (count @logger-buffer))
        (rebase state-atom prompt-filename error? true)
        (js/setTimeout #(do (-> term
                                (.column 0)
                                (prompt prompt-filename
                                        (get-in @state-atom [:colorset :color3])
                                        (get-in @state-atom [:colorset :color2])))) 10)))))

(defn unsafe-log [state-atom msg & [error-flag]]
  ((fn [msg]
     (logger state-atom (:filename @state-atom)
             msg
             (or error-flag false)))
   msg))

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
      ;; No op, falseful information
      (re-find #"Reading options from" msg) 
      (recur msgs out token-error end-of-score overall-samps plain-error)
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

(defn flush-logger-buffer [state-atom]
  (let [filename (:filename @state-atom)]
    (js/setTimeout
     #(do (reset! logger-buffer (repair-logs logger-buffer))
          (logger state-atom
                  filename
                  (ffirst @logger-buffer)
                  (second (first @logger-buffer)))
          (swap! logger-buffer (comp vec rest)) ) 10)))
