(ns csnd.global-keybindings
  (:require [csnd.term :refer [term prompt exit-gracefully]]
            [csnd.logger :refer [unsafe-log]]
            [csnd.utils :as utils]))

(defn set-global-keys [filename state-atom]
  (.on term "key"
       (fn [name matches data]
         (let [input-stream (:current-input-stream @state-atom)
               cur-line     (and input-stream (.getInput input-stream))]
           (case name
             ("ESCAPE" "CTRL_C")
             (when input-stream
               (.deleteLine term 1))
             "CTRL_Q"
             (utils/exit-prompt state-atom term #(exit-gracefully state-atom 0))
             "CTRL_D"
             (when (empty? cur-line)
               (when input-stream
                 (let [input-field-location (.getPosition input-stream)
                       bottom?              (>= (.-y input-field-location)
                                                (.-height term))]
                   (.abort input-stream)
                   (when bottom?
                     (-> term (.previousLine 1) (.deleteLine 1)))))
               ;; (.deleteLine term 0)
               (exit-gracefully state-atom 0))
             "CTRL_L"
             (let [original-xpos (.-x (.getPosition input-stream))]
               (.clear term)
               (.rebase input-stream 0 0)
               (prompt term filename
                       (get-in @state-atom [:colorset :color3])
                       (get-in @state-atom [:colorset :color2]))
               (.rebase input-stream original-xpos 0))
             "CTRL_P"
             (when (and (:image @state-atom) (get-in @state-atom [:image :path]))
               (let [image         (:image @state-atom)
                     original-xpos (.-x (.getPosition input-stream))]
                 (.deleteLine term 1)
                 (.nextLine term 1)
                 (-> (.drawImage term (utils/expand-homedir (:path image))
                                 #js {:shrink
                                      #js {:width  (* (:width image) (.-width term))
                                           :height (* (:height image) (.-height term))}})
                     (.then (fn []
                              (.moveTo term 0 (.-height term))
                              (prompt term filename
                                      (get-in @state-atom [:colorset :color3])
                                      (get-in @state-atom [:colorset :color2]))
                              (.rebase input-stream original-xpos (.-height term))
                              (when-let [img-title (:title image)]
                                (unsafe-log state-atom img-title :tab_filled1)))))))
             nil))
         #_(-> (.getCursorLocation term)
               (.then (fn [location]
                        (when location
                          (let [x        (if location (.-x location) 0)
                                y        (if location (.-y location) 0)
                                cur-line (:currentLine @state-atom)]
                            (swap! state-atom assoc :currentLine
                                   (case name
                                     "BACKSPACE"
                                     (if (empty? cur-line)
                                       ""
                                       (subs cur-line 0 (dec (count cur-line))))
                                     ("ESCAPE" "CTRL_C") ""
                                     ;; ELSE
                                     (if (.-isCharacter data)
                                       (str cur-line name)
                                       cur-line))))))))
         
         #_(prn "name" name
                "matches" matches
                "data" (.toString (.-code data))))))
