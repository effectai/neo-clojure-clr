(ns neo-clj.implementations
  (:import [Neo.Core Blockchain Block]))

(gen-class
 :name "neo_clj.implementations.ExtendedLevelDBBlockchain"
 :extends Neo.Implementations.Blockchains.LevelDB.LevelDBBlockchain
 :prefix "-")

(defn -OnPersistCompleted ^void [this ^Block block])
