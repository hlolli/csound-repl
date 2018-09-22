(ns csnd.utils
  (:require [csnd.term :refer [term prompt exit-gracefully]]
            [csnd.logger :as logger]
            [csnd.repl :as repl]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [fipp.edn :refer (pprint) :rename {pprint fipp}]
            ["path" :as path]
            ["os" :as os]))

(defn expand-homedir [path-str]
  (-> (.replace path-str "~" (os/homedir))
      (path/resolve)))

(defn exit-prompt [state-atom term cb]
  (let [opt #js {:yes ["y", "ENTER"] :no ["n"]}
        cb' (fn [err bool]
              ;; (logger/unsafe-log state-atom (str "Err: " err " bool: " bool))
              (if bool (cb)
                  (do (prompt term (:filename @state-atom)
                              (get-in @state-atom [:colorset :color3])
                              (get-in @state-atom [:colorset :color2]))
                      (repl/start-repl state-atom))))]
    (.deleteLine term 0)
    (term "Would you like to quit csound-cli? [Y|n] ")
    ;; (logger/unsafe-log state-atom "Would you like to quit csound-cli [Y|n]")
    (when-let [input-stream (:current-input-stream @state-atom)]
      (.abort input-stream))
    (.yesOrNo term opt cb')))

(defn assert-and-explain
  "returns true/false"
  [state-atom spec data exit-on-fail?]
  (if-not (s/valid? spec data)
    (let [explain-chunks
          (-> (with-out-str (pprint (s/explain-data spec data)))
              (string/split #"\n"))]
      (doall (for [chnk explain-chunks]
               (swap! logger/logger-buffer conj [chnk :warn_notrim])))
      (when exit-on-fail?
        (logger/flush-logger-buffer state-atom)
        (js/setTimeout #(exit-gracefully state-atom 1)
                       (* 5 (count @logger/logger-buffer))))
      false)
    true))
