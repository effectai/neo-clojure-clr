(ns tasks
  (:require
   neo-clj.core neo-clj.blockchain neo-clj.util neo-clj.crypto
   [nostrand.repl :as repl])
  (:import System.IO.Directory))

(defn build []
  (let [build-dir "build"]
    (when-not (Directory/Exists build-dir)
      (println "Creating build directory")
      (Directory/CreateDirectory build-dir))
    (binding [*compile-path* build-dir]
      (compile 'neo-clj.blockchain)
      (compile 'neo-clj.util)
      (compile 'neo-clj.crypto)
      (compile 'neo-clj.core))))

(defn repl []
  (repl/repl 11217))

