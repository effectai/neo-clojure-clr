(assembly-load "neo_clj.implementations.ExtendedLevelDBBlockchain")
(assembly-load "neo_clj.implementations.ExtendedStateMachine")
(ns tasks
  (:require
   [neo-clj.rpc :as rpc]
   [neo-clj.blockchain :as blockchain]
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

(defn repl []
  (repl/repl 11217))

(defn rpc []
  (let [port 10336
        b (blockchain/create)
        server (rpc/create-server {:port port})]
    (rpc/start-server server)))
