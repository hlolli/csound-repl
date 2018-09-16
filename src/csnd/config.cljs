(ns csnd.config
  (:require [csnd.colors :as colors]
            ["os" :as os]
            ["path" :as path]
            ["fs" :as fs]
            ["../js/mkDir" :default mkDir]
            [clojure.tools.reader.edn :as edn]))


(def default-config-to-spit
  (prn-str {:autopair true
            :colorset "jetoma"}))

(def cfg-dir     (path/join (os/homedir) ".csnd"))
(def history-loc (path/join cfg-dir "history.edn"))
(def config-loc (path/join cfg-dir "config.edn"))

(defn load-configuration! [state-atom]
  (mkDir cfg-dir)
  (when-not (fs/existsSync config-loc)
    (fs/writeFileSync config-loc default-config-to-spit))
  (when (fs/existsSync config-loc)
    (swap! state-atom merge
           (try (edn/read-string
                 (.toString (fs/readFileSync config-loc)))
                (catch js/Error e (do (.error
                                       js/console
                                       (str "WARNING broken or invalid edn file: "
                                            config-loc ".\n"
                                            e))
                                      {})))))
  (colors/colorset-from-config state-atom (:colorset @state-atom)))

(defn load-history! [history-atom]
  (mkDir cfg-dir)
  (when (fs/existsSync history-loc)
    (reset! history-atom
            (try (edn/read-string
                  (.toString (fs/readFileSync history-loc)))
                 (catch js/Error e [])))))

(defn update-history! [new-item history-atom]
  (let [new-history (conj (vec (take 49 @history-atom)) new-item)]
    (reset! history-atom new-history)
    (fs/writeFileSync history-loc new-history)))
