(ns tasks
  (:require
   neo-clj.core
   [neo-clj.wallet :as wallet]
   [neo-clj.blockchain :as bc]
   neo-clj.util
   neo-clj.crypto
   [neo-clj.rpc :as rpc]
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
    (compile 'neo-clj.rpc)
    (compile 'neo-clj.crypto)
    (compile 'neo-clj.core)))

(defn build-aot []
  (ensure-build-dir!)
  (binding [*compile-path* build-dir]
    (compile 'neo-clj.implementations))
  (assembly-load "neo_clj.implementations.ExtendedLevelDBBlockchain"))

(defn repl []
  (build-aot)
  (repl/repl 11217))

(defn rpc []
  (build-aot)
  (let [port 10336
        b (bc/create)
        server (rpc/create-server {:port port})]
    (rpc/start-server server))
  (repl/repl 11217))
