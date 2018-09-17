(ns csnd.utils
  (:require [csnd.term :refer [term prompt]]
            [csnd.logger :as logger]
            [csnd.repl :as repl]
            ["path" :as path]
            ["os" :as os]))

(defn expand-homedir [path-str]
  (-> (.replace path-str "~" (os/homedir))
      (path/resolve)))

;; var term = require( 'terminal-kit' ).terminal ;

;; var items = [ 'File' , 'Edit' , 'View' , 'History' , 'Bookmarks' , 'Tools' , 'Help' ] ;

;; var options = {
;; 	y: 1 ,	// the menu will be on the top of the terminal
;; 	style: term.inverse ,
;; 	selectedStyle: term.dim.blue.bgGreen
;; } ;

;; term.clear() ;

;; term.singleLineMenu( items , options , function( error , response ) {
;; 	term( '\n' ).eraseLineAfter.green(
;; 		"#%s selected: %s (%s,%s)\n" ,
;; 		response.selectedIndex ,
;; 		response.selectedText ,
;; 		response.x ,
;; 		response.y
;; 	) ;
;; 	process.exit() ;
;; } ) ;

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
