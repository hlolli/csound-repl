(ns csnd.banner
  (:require [csnd.term :refer [term]]
            ["fs" :as fs]
            ["path" :as path]))

(def package-json
  (js/JSON.parse
   (.toString
    (fs/readFileSync (path/join (.cwd js/process) "package.json")))))

(def csnd-version (.-version package-json))

(defn generate-banner [csound-version]
  (let [csound-version (str csound-version)]
    [[(str "csnd-cli " csnd-version) false]
     [(str "Csound "
           (subs csound-version 0 1) "."
           (subs csound-version 1 3) "."
           (subs csound-version 3 4))
      false]]))