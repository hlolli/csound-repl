(ns csnd.core
  (:require [csnd.colors :as colors]
            [csnd.repl :as repl]
            [csnd.term :refer [term prompt exit-gracefully]]
            [csnd.config :as config]
            [csnd.logger :refer [logger logger-buffer unsafe-log 
                                 flush-logger-buffer repair-logs]]
            [csnd.global-keybindings :as global-keybindings]
            [csnd.utils :as utils]
            ["csound-api" :as csound]
            ["argparse" :as args]
            ["path" :refer [basename] :as path]
            ["fs" :as fs]
            [clojure.string :as string] 
            [clojure.spec.alpha :as s]))

;; csound.DisposeOpcodeList(Csound, opcodeList);

(def state (atom {:current-input-stream nil
                  :csound-instance      nil
                  :opcode-symbols       []
                  :history              []
                  :filename             ""
                  :filepath             ""
                  :colorset             {}
                  :active-watchers      {}}))

;; (def csound-instance (atom nil))

(.SetDefaultMessageCallback csound (fn []))

;; (def opcode-symbols (atom []))

;; (def history-atom (atom []))

(defn get-opcode-symbols []
  (let [js-arr #js []
        Csound (.Create csound "-m0")]
    (.SetMessageCallback csound Csound (fn []))
    (doto Csound
      ((.-NewOpcodeList csound) js-arr))
    ((.-Destroy csound) Csound)
    (let [sym-arr (amap js-arr indx ret
                        (-> (js/Object.values
                             (aget js-arr indx))
                            (aget 2)))]
      (->> (js->clj sym-arr)
           (remove empty?)
           (remove #{"sr" "poscil"})))))

#_(defn logger [prompt-filename msg error?]
    (let [terminal-width (.-width term)
          chopped-msg    (string/split msg "\n")
          #_             (map (partial apply str)
                              (partition-all terminal-width msg))]
      (run! #(logger* prompt-filename % error?)
            chopped-msg)))


(defn start-csound [state-atom]
  (let [Csound        (.Create csound "-m0")
        filename      (:filename @state-atom)
        _             (.SetDefaultMessageCallback
                       csound (fn [code msg]
                                (when-not (empty? (string/trim msg))
                                  (when (empty? @logger-buffer)
                                    (flush-logger-buffer state-atom))
                                  (swap! logger-buffer conj
                                         [msg (not= 0 code)]))))
        file-contents (fs/readFileSync (:filepath @state-atom))]
    ;; MAJOR BUG, report asap
    #_(when-not (empty? (:include-dirs @state-atom))
        (run! #(.SetOption csound Csound (str "--env:INCDIR+=" %))
              (:include-dirs @state-atom)))
    ;; (.SetOption csound Csound "-+ignore_csopts=1")
    ;; (.SetOption csound Csound "--postscriptdisplay")
    (.SetOption csound Csound "--output=dac")
    (.SetOption csound Csound (str "--ksmps=" (:ksmps @state-atom)))
    (.SetOption csound Csound (str "--0dbfs=" (:zerodbfs @state-atom)))
    (.SetOption csound Csound (str "--nchnls=" (:nchnls @state-atom)))
    (case (path/extname filename)
      (".udo" ".orc") (js/setTimeout #(.CompileOrc csound Csound file-contents) 10)
      (do (logger state
                  filename
                  (str (path/basename (:filepath @state-atom))
                       " is not a valid csound file.")
                  true)
          (exit-gracefully state 1)))
    ;; (js/setTimeout #(.Message csound Csound "hello 100000000001 100000000000000001 999") 1000)
    ;; (js/setTimeout #(.Message csound Csound "hello 2") 2000)

    (swap! state assoc :csound-instance Csound)
    (if (= (.-SUCCESS csound) (.Start csound Csound))
      (.PerformAsync csound Csound #(.Destroy csound Csound))
      (logger state filename "Csound couldn't be started" true))))


(def argument-parser
  (new (.-ArgumentParser args)
       #js {:version     "1.0.0-alpha"
            :addHelp     "true"
            :description "Csound terminal REPL"}))

(defn add-argument [argument config]
  (.addArgument argument-parser
                argument config))

(add-argument "file"
              #js {:help "A csound file (csd/orc/sco)"
                   :type "string"})

(add-argument #js ["-k" "--ksmps"]
              #js {:help         "A global ksmps value"
                   :type         "int"
                   :defaultValue 64})

(add-argument #js ["-A" "--zerodbfs"]
              #js {:help         "The full-scale of zero decibel"
                   :type         "int"
                   :defaultValue 1})

(add-argument #js ["-c" "--nchnls"]
              #js {:help         "Number of output channels"
                   :type         "int"
                   :defaultValue 2})

(add-argument #js ["-x" "--on-change"]
              #js {:help "Orchestra code to compile when file changes"
                   :type "string"})

(add-argument #js ["-I" "--incdir"]
              #js {:help (str "Directories that will be recursively compiled.\n"
                              "When given as cli argument, it must be a JSON string,\n"
                              "ex. --incdir \"[\\\"/path/to/dir\\\"]\"" )
                   :type "string"})

(defn remove-temp-files [filevec]
  (vec (remove #(or (.endsWith % "~") (.includes % "#")) filevec)))

#_(defn- strip-and-determine-included-files
    "returns vectore with uncommented included 
   filenames and a string where the includes 
   have been trimmed out.
   This is needed due to reload bug with 
   #include. The include statements are already
   called once at this poins, subsequential
   include calls must be trimmed out."
    [filepath]
    (let [file-contents (try (.toString (fs/readFileSync filepath))
                             (catch js/Error e nil))]
      (when file-contents
        (loop [[line & lines]  (string/split file-contents "\n")
               inside-comment? false
               filenames       []
               chunks          []]
          (if (nil? line)
            [filenames (string/join "\n" chunks)]
            (let [comment-ends?   (if inside-comment? (some? (re-find #"\*/" line)) nil)
                  line-trim       (if (and inside-comment? comment-ends?)
                                    (let [comment-end-index (.indexOf line "*/")
                                          subs-index        (if (< comment-end-index 0)
                                                              0 (+ 2 comment-end-index))]
                                      (subs line subs-index))
                                    line)
                  include?        (if inside-comment? 
                                    false
                                    (some? (re-matches #"[ \t]*[^;|^/\*]*#include.*\".*\"" line-trim)))
                  inside-comment? (if (and inside-comment? (not comment-ends?))
                                    false true)]
              (recur lines
                     inside-comment?
                     (if-not include? 
                       filenames 
                       (conj filenames (second (re-find #"#include.*\"(.*)\"" line))))
                     (if include? 
                       (conj chunks (string/replace line #"#include.*\".*\"" ""))
                       (conj chunks line)))))))))

(defn file-watcher [state-atom file main?]
  (let [base-filename (path/basename file)
        throttle      (volatile! false)]
    (fs/watch file #js {:persistent true}
              (fn [event-type filename]
                (let [file-contents (try (.toString (fs/readFileSync file))
                                         (catch js/Error e nil))
                      ;;[_ file-contents] (strip-and-determine-included-files file)
                      Csound        (:csound-instance @state-atom)]
                  (when (and (= "change" event-type) (not @throttle)
                             (not (empty? file-contents)))
                    (vreset! throttle true)
                    (js/setTimeout #(vreset! throttle false) 50)
                    (when (and main? (empty? @logger-buffer))
                      (flush-logger-buffer state-atom))
                    (swap! logger-buffer conj
                           [(str "--> Changes detected "
                                 (if main? "in " "from an included file ")
                                 base-filename
                                 " recompiling...") false])
                    (fs/readFile file
                                 (fn [err contents]
                                   (if err
                                     (js/console.error
                                      (str "Error reading file: " err))
                                     ;; (js/console.log Csound)
                                     (.CompileOrc csound Csound (.toString contents))
                                     #_(.CompileOrc csound Csound contents))))
                    #_(when-let [on-change-code (:on-change @state-atom)]
                        (.CompileOrc csound Csound on-change-code))))))))

(defn folder-watcher [state-atom folder]
  (let [base-filename (path/basename folder)
        dir-items     (-> (js->clj (fs/readdirSync folder))
                          remove-temp-files)]
    (->> dir-items
         (run! (fn [filename]
                 (let [filepath (path/join folder filename)]
                   (if (.isDirectory (fs/lstatSync filepath))
                     (folder-watcher state-atom filepath)
                     (when (and (#{".udo" ".orc" ".csd"} (path/extname filepath))
                                (not 
                                 (contains?
                                  (into #{} (keys (:active-watchers @state-atom))) filepath)))
                       (fs/readFile
                        filepath
                        (fn [err contents]
                          (if err
                            (js/console.error "Error loading" filename "\n" err "\n")
                            (.CompileOrc csound (:csound-instance @state-atom)
                                         (.toString contents)))))
                       (swap! logger-buffer conj
                              [(str "--> Watching included file "
                                    filepath
                                    " for changes") false])
                       (file-watcher state-atom filepath false)
                       
                       #_(->>
                          (file-watcher state-atom filepath false)
                          (swap! state-atom assoc-in [:active-watchers filepath]))))))))))


(defn every-element-string? [array] (every? string? array))
(defn every-path-is-folder? [array] (-> #(.isDirectory (fs/lstatSync %))
                                        (every? array)))

(s/def ::incdir (s/and vector?
                       every-element-string?
                       every-path-is-folder?))

(defn resolve-include-dirs [state-atom args]
  (let [incdir-cli-raw       (.-incdir args)
        incdir-cli           (if-not (empty? incdir-cli-raw)
                               (try (js->clj (js/JSON.parse incdir-cli-raw))
                                    (catch js/Error e 
                                      (do (js/console.error 
                                           "Unparseable argument to incdir"
                                           (prn-str incdir-cli-raw)
                                           "Should be a valid JSON string.")
                                          (exit-gracefully state-atom 1))))
                               [])
        incdir-cli           (mapv (comp path/resolve utils/expand-homedir) incdir-cli)
        incdir-cli-valid?    (utils/assert-and-explain state-atom ::incdir
                                                       incdir-cli true)
        config-incdir        (or (:include-dirs @state-atom) [])
        config-incdir        (mapv (comp path/resolve utils/expand-homedir) config-incdir)
        config-incdir-valid? (utils/assert-and-explain state-atom ::incdir
                                                       config-incdir true)
        out-vec              (-> (into incdir-cli config-incdir)
                                 dedupe vec)]
    (swap! state-atom assoc :include-dirs out-vec)
    (if (or (not incdir-cli-valid?) (not config-incdir-valid?))
      false true)))

(defn main [& _args]
  (let [args     (.parseArgs argument-parser)
        filename (basename (.-file args))
        abs-path (path/normalize
                  (path/resolve
                   (.-file args)))]
    (swap! state assoc 
           :filename filename 
           :filepath abs-path
           :ksmps (.-ksmps args)
           :nchnls (.-nchnls args)
           :zerodbfs (.-zerodbfs args)
           :on-change (if (empty? (.-on-change args))
                        nil (.-on-change args)))
    (config/load-configuration! state)
    (config/load-history! state)
    (when-not (fs/existsSync (.-file args))
      (colors/error-color
       (str "File " abs-path " doesn't exist!"))
      (exit-gracefully state 1))
    ;; (js/console.log args)
    (.windowTitle term "Csnd - Csound REPL")
    (global-keybindings/set-global-keys filename state)
    (swap! state update :opcode-symbols into (get-opcode-symbols))
    (when (resolve-include-dirs state args)
      (start-csound state)
      (run! #(folder-watcher state %) (:include-dirs @state))
      (file-watcher state abs-path true)
      (repl/start-repl state)
      (prompt term filename
              (get-in @state [:colorset :color3])
              (get-in @state [:colorset :color2])))))

