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

;; (defn -OnPersistCompleted ^void [this ^Block block])

(gen-class
 :name "neo_clj.implementations.ExtendedStateMachine"
 :extends Neo.SmartContract.StateMachine
 :prefix "-SM-"
 :init init
 :constructors {[|Neo.IO.Caching.DataCache`2[Neo.UInt160,Neo.Core.AccountState]|
                 |Neo.IO.Caching.DataCache`2[Neo.Cryptography.ECC.ECPoint,Neo.Core.ValidatorState]|
                 |Neo.IO.Caching.DataCache`2[Neo.UInt256,Neo.Core.AssetState]|
                 |Neo.IO.Caching.DataCache`2[Neo.UInt160,Neo.Core.ContractState]|
                 |Neo.IO.Caching.DataCache`2[Neo.Core.StorageKey,Neo.Core.StorageItem]|]
                 [|Neo.IO.Caching.DataCache`2[Neo.UInt160,Neo.Core.AccountState]|
                 |Neo.IO.Caching.DataCache`2[Neo.Cryptography.ECC.ECPoint,Neo.Core.ValidatorState]|
                 |Neo.IO.Caching.DataCache`2[Neo.UInt256,Neo.Core.AssetState]|
                 |Neo.IO.Caching.DataCache`2[Neo.UInt160,Neo.Core.ContractState]|
                 |Neo.IO.Caching.DataCache`2[Neo.Core.StorageKey,Neo.Core.StorageItem]|]}
 :post-init post-init
 :methods [[GetCreatedContracts []
            |System.Collections.Generic.Dictionary`2[Neo.UInt160,Neo.UInt160]|]])

(defn -SM-init [accounts validators assets contracts storages]
  (println "pre-init")
  [[accounts validators assets contracts storages] nil])

(defn -SM-post-init [this accounts validators assets contracts storages]
  (println "post-init")
  )

(defn -SM-GetCreatedContracts [this]
  (get-private-field this "contracts_created"))


