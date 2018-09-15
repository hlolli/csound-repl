(ns csnd.term
  (:require ["terminal-kit" :as terminal]
            ["terminal-kit-plugins" :as terminal-plugins]))

(def term (.-terminal terminal))
(.plugin terminal-plugins term)
