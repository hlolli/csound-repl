(ns csnd.term
  (:require ["terminal-kit" :as terminal]
            ["csound-api" :as csound]
            #_["terminal-kit-plugins" :as terminal-plugins]))

(def term (.-terminal terminal))
;; (.plugin terminal-plugins term)

(defn prompt [term filename filecolor promptcolor]
  (-> term
      (.colorRgbHex.bold
       filecolor
       (str "[" filename "] "))
      (.colorRgbHex promptcolor "csnd> ")
      (.styleReset)))

(defn multiline-prompt [term filename filecolor promptcolor indent-level]
  (-> term
      (.column 0)
      (.colorRgbHex.bold
       filecolor
       (str " " (apply str (take (count filename) (cycle [" "])))
            "      "))
      (.colorRgbHex promptcolor ">")
      (.styleReset)
      (as-> term (term (str " " (apply str (take indent-level (cycle [" "]))))))))


(defn exit-gracefully [state exit-code]
  (when-let [Csound (:csound-instance @state)]
    (.Stop csound Csound)
    (.Destroy csound Csound))
  (.processExit term exit-code))
