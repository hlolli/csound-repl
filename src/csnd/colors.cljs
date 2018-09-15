(ns csnd.colors
  [:require [csnd.term :refer [term]]])

(defn error-color [string]
  ((.colorRgbHex
    (.bold term)
    "#FF0000")
   string))

(def cyberpunk-neon
  {:color1 "#711c91"
   :color2 "#ea00d9"
   :color3 "#0abdc6"
   :color4 "#133e7c"
   :color5 "#091833"})

(def current-colorset
  (atom cyberpunk-neon))

(defn csound-log-color [string]
  (str ((.colorRgbHex
         (.dim term)
         (:color2 @current-colorset))
        string)
       (.styleReset term)))

(defn csound-log-color-error [string]
  (str ((.colorRgbHex
         (.dim term)
         "#FF0000")
        string)
       (.styleReset term)))


#_(defn newline []
    (str ((.colorRgbHex
           (.inverse term)
           (:color2 @current-colorset))
          string)
         (.styleReset term)))

