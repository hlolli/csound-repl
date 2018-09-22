(ns csnd.colors
  (:require [csnd.term :refer [term]]
            [csnd.logger :refer [logger-buffer]]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [fipp.edn :refer (pprint) :rename {pprint fipp}]))

;; (def current-colorset (atom {}))

(s/def ::hexcolor (s/and string? #(re-matches #"#[0-9a-f]{6}" %)))
(s/def ::color1 ::hexcolor)
(s/def ::color2 ::hexcolor)
(s/def ::color3 ::hexcolor)
(s/def ::color4 ::hexcolor)
(s/def ::color5 ::hexcolor)

(s/def ::colorset (s/keys :req-un [::color1 ::color2 ::color3 ::color4 ::color5]))

(defn error-color [string]
  ((.colorRgbHex
    (.bold term)
    "#FF0000")
   string))


;; Some sets from
;; https://www.color-hex.com
(def cyberpunk
  {:color1 "#00ff9f"
   :color2 "#00b8ff"
   :color3 "#001eff"
   :color4 "#bd00ff"
   :color5 "#d600ff"})

(def watermelon
  {:color1 "#853037"
   :color2 "#ff3d4e"
   :color3 "#d25f6c"
   :color4 "#143d1d"
   :color5 "#387a4a"})

(def jetoma
  {:color1 "#17c723"
   :color2 "#c1d23a"
   :color3 "#6f3fce"
   :color4 "#9524a3"
   :color5 "#9bbd3b"})

(def happy
  {:color1 "#adb4cc"
   :color2 "#4ed1c8"
   :color3 "#e9275e"
   :color4 "#017467"
   :color5 "#fc5f09"})

(defn csound-log-color [string]
  (str (.styleReset term)
       (.defaultColor term string)
       (.styleReset term)))

(defn csound-log-color-error [string]
  (str ((.colorRgbHex
         (.dim term)
         "#FF0000")
        string)
       (.styleReset term)))

(defn colorset-from-config [state-atom colorset]
  (let [colorset-str (string/replace (str colorset) ":" "")]
    (if (map? colorset)
      (if (s/valid? ::colorset colorset)
        (swap! state-atom assoc :colorset colorset)
        (let [explain-chunks (-> (with-out-str (pprint (s/explain-data ::colorset colorset)))
                                 (string/split #"\n"))]
          (swap! state-atom assoc :colorset cyberpunk)
          (doall (for [chnk explain-chunks]
                   (swap! logger-buffer conj [chnk :warn_notrim])))))
      (swap! state-atom assoc :colorset
             (case colorset-str
               "cyberpunk"  cyberpunk
               "jetoma"     jetoma
               "watermelon" watermelon
               "happy"      happy
               cyberpunk)))))

#_(defn newline []
    (str ((.colorRgbHex
           (.inverse term)
           (:color2 @current-colorset))
          string)
         (.styleReset term)))

