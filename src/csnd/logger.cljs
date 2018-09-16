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
        msg                  (-> msg (string/replace "(token \"\n" "(token \"\\n"))
        msg                  (string/replace msg "\n" " ")
        msg                  (if (re-matches #".*notrim.*" (str error?))
                               msg
                               (string/trim msg))
        msg                  (if (= :tabbed error?) (str " " msg) msg)
        ;; msg                  (prn-str msg)
        ]
    (-> term
        (as-> term (term "\n"))
        (.previousLine 1)
        (.eraseLine)
        (.styleReset)
        (as-> term
            (cond
              (re-matches #".*warn.*" (str error?))
              (.colorRgbHex term (get-in @state-atom [:colorset :color5]))
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
