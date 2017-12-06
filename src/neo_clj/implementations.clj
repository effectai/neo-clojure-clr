(ns neo-clj.implementations
  (:import
   System.Reflection.BindingFlags
   [Neo UInt160]
   [Neo.Core Blockchain Block]
   Neo.SmartContract.StateMachine))

(defn get-private-field
  "Get a private field from an object using reflection"
  [obj field-name]
  (let [type (.GetType obj)
        field (.GetField type field-name (bit-or BindingFlags/NonPublic BindingFlags/Instance))]
    (.GetValue field obj)))

(gen-class
 :name "neo_clj.implementations.ExtendedLevelDBBlockchain"
 :extends Neo.Implementations.Blockchains.LevelDB.LevelDBBlockchain
 :prefix "-")

(defn -OnPersistCompleted ^void [this ^Block block])
