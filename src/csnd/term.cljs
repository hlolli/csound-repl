(ns csnd.term
  (:require ["terminal-kit" :as terminal]
            ["terminal-kit-plugins" :as terminal-plugins]))

(def term (.-terminal terminal))
(.plugin terminal-plugins term)

(defn prompt [term filename filecolor promptcolor]
  (-> term
      (.colorRgbHex.bold
       filecolor
       (str "[" filename "] "))
      (.colorRgbHex promptcolor "csnd> ")
      (.styleReset)))
