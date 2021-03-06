(ns csnd.banner
  (:require [csnd.term :refer [term]]
            ["fs" :as fs]
            ["path" :as path]))

(def package-json
  (js/JSON.parse
   (.toString
    (fs/readFileSync
     (path/resolve
      (path/join js/__dirname "../package.json"))))))

(def csnd-version (.-version package-json))

(defn generate-banner [csound-version]
  (let [csound-version (str csound-version)
        pre-postfix    ["\t===================" false]]
    [["\n" false]
     [(str "Csound "
           (subs csound-version 0 1) "."
           (subs csound-version 1 3) "."
           (subs csound-version 3 4))
      :banner]
     [(str "Csnd " csnd-version) :banner]
     ["Exit: Control+D" :banner_tab]
     ["\n" false]]))
