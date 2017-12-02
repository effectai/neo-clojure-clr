(ns tasks
  (:require
   neo-clj.core neo-clj.blockchain neo-clj.util neo-clj.crypto
   [nostrand.repl :as repl])
  (:import System.IO.Directory))

(def build-dir "build")

(defn ensure-build-dir! []
  (when-not (Directory/Exists build-dir)
    (println "Creating build directory")
    (Directory/CreateDirectory build-dir)))

(defn build []
  (ensure-build-dir!)
  (binding [*compile-path* build-dir]
    (compile 'neo-clj.blockchain)
    (compile 'neo-clj.util)
    (compile 'neo-clj.crypto)
    (compile 'neo-clj.core)))

(defn build-aot []
  (ensure-build-dir!)
  (binding [*compile-path* build-dir]
    (compile 'neo-clj.implementations)))

(defn repl []
  (ensure-build-dir!)
  ;; Load AOT assemblies
  (build-aot)
  (assembly-load "neo_clj.implementations.ExtendedLevelDBBlockchain")
  (repl/repl 11217))
