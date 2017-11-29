(ns neo-clj.core
  (:require
   [neo-clj.wallet :as wallet]
   [neo-clj.crypto :as crypto]
   [neo-clj.blockchain :as blockchain])
  (:import
   [System BitConverter Convert Array]
   System.Text.Encoding
   System.IO.File
   Neo.Implementations.Wallets.EntityFramework.UserWallet
   [Neo.Core IVerifiable Witness Blockchain ClaimTransaction
    CoinReference Transaction InvocationTransaction StorageKey
    StorageItem]
   Neo.Cryptography.Crypto
   [Neo.VM ScriptBuilder VMState ExecutionEngine]
   [Neo.SmartContract ContractParametersContext Contract
   ContractParameterType ApplicationEngine]
   [Neo.Wallets Wallet VerificationContract Coin]
   [Neo Helper Fixed8 UInt256 Settings])
  (:gen-class :main true))

(def settings Settings/Default)

(def public-keys (.StandbyValidators settings))

(defn claim-initial-neo-tx
  "Create a signed raw transaction that claims the NEO from genesis block"
  [address]
  (let [to-address (wallet/pub-key-to-address address)
        wallets (map-indexed
                 #(let [wal (wallet/create (str "wal" %1) "test")]
                    (.Import wal %2) wal) wallet/wifs)]
    (dorun (map wallet/add-multi-sig-contract wallets keys))      ; add the multi-sig tx
    (-> wallets first (.Rebuild))                                 ; rebuild wallet balances
    (while (-> wallets first (.GetCoins) empty?) -1)              ; wait for rebuild...
    (let [tx (wallet/make-transaction
              (first wallets) to-address "100000000")
          signs (dorun (map #(wallet/sign-context % (:ctx tx)) wallets))]
      (if (not (.Completed (:ctx tx)))
        (throw (Exception. "Failed to sign the transaction"))
        {:tx (:tx tx)
         :ctx (:ctx tx)}))))

(defn deploy-contract-tx
  [wallet {:keys [file params return needs-storage version author email description]}]
  (let [script (File/ReadAllBytes file)
        sb (ScriptBuilder.)]
    (doto sb
      (.EmitPush (str description))
      (.EmitPush (str email))
      (.EmitPush (str author))
      (.EmitPush (str version))
      (.EmitPush (str name))
      (.EmitPush (boolean needs-storage))
      (.EmitPush (System.Numerics.BigInteger/Parse return))
      (.EmitPush (Helper/HexToBytes params))
      (.EmitPush script)
      (.EmitSysCall "Neo.Contract.Create"))
    (let [tx (InvocationTransaction.)]
      (set! (.Version tx) 1)
      (set! (.Script tx) (.ToArray sb))
      (let [engine (ApplicationEngine/Run (.Script tx) tx)]
        (set! (.Gas tx) (InvocationTransaction/GetGas (.GasConsumed engine)))
        (let [ctx (-> (.MakeTransaction wallet tx nil Fixed8/Zero)
                      (ContractParametersContext.))]
          {:tx tx
           :script (.ToArray sb)
           :ctx ctx
           :script-hash (Neo.Core.Helper/ToScriptHash script)})))))

(defn invoke-contract-tx [wallet script-hash]
  (let [sb (ScriptBuilder.)]
    (doto sb
      (.EmitPush (make-array String 0))
      (.EmitAppCall (.ToArray script-hash) false))
    (let [tx (InvocationTransaction.)]
      (set! (.Version tx) 1)
      (set! (.Script tx) (.ToArray sb))
      (let [ctx (-> (.MakeTransaction wallet tx nil (Fixed8/Zero))
                    (ContractParametersContext.))]
        {:tx tx
         :ctx ctx}))))

(defn -main []
  (println "Hello World")
  (blockchain/create!))
