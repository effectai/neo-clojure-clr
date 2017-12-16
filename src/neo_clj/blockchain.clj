(ns neo-clj.blockchain
  (:refer-clojure :exclude [sync])
  (:require
   [clojure.walk :refer [keywordize-keys]]
   [neo-clj.util :as util]
   [neo-clj.implementations :refer [get-private-field]]
   [clojure.data.json :as json]
   [clojure.pprint :refer [pprint]])
  (:import
   System.Text.Encoding
   System.Net.WebRequest
   System.Threading.Thread
   Neo.VM.VMState
   System.Environment
   [System.IO BinaryReader MemoryStream File StreamReader]
   [Neo Helper UInt256 UInt160 Fixed8]
   [Neo.Cryptography.ECC ECCurve ECPoint]
   System.Reflection.BindingFlags
   [Neo.SmartContract CachedScriptTable StateMachine ApplicationEngine
    TriggerType]
   [Neo.Core Blockchain Block
    AccountState ValidatorState AssetState ContractState
    StorageKey StorageItem]
   [Neo.Implementations.Blockchains.LevelDB DB LevelDBBlockchain]
   [Neo.Network LocalNode]))

(def chain-path "./Chain")

;; List of RPC servers used for syncing
(def rpc-list [(util/get-env "RPC_SERVER" "http://localhost:10332")])

(def utility-token Blockchain/UtilityToken)

(def asset-ids
  {:neo (UInt256/Parse "c56f33fc6ecfcd0c225c4ab356fee59390af8560be0e930faebe74a6daff7c9b")
   :gas (UInt256/Parse "602c79718b16e442de58778e148d0b1084e3b2dffd5de6b7b16cee7969282de7")})

(def tx-type {:miner "MinerTransaction"
              :register "RegisterTransaction"
              :issue "IssueTransaction"
              :invocation "InvocationTransaction"})

(def state (atom {:assets [] :contracts []}))

(defn- obj->clj
  "Transform an object that support 'ToJson' method into a map"
  [object]
  (-> (json/read-str (-> object .ToJson str))
      (assoc-in [:object] object)
      keywordize-keys))

(defn on-block-persist [callback-fn]
  (Blockchain/add_PersistCompleted
   (gen-delegate |System.EventHandler`1[[Neo.Core.Block]]|
                 [sender block] (callback-fn sender (obj->clj block)))))

(defn- update-state [{transactions :tx :as block}]
  (let [assets
        (->> transactions
             (filter #(= (:type %) (:register tx-type)))     ; all register txs
             (map #(assoc-in % [:asset :txid] (:txid %)))    ; enhance :asset with the :txid
             (map :asset))]                                  ; get list of assets
    (swap! state (fn [s] (update-in s [:assets] #(concat % assets))))))

(defn get-block
  "Get a block and transform data into a clojure map. Original block
  is stored under the :object key"
  ([n] (get-block Blockchain/Default n))
  ([bc n] (obj->clj (.GetBlock bc n))))

(defn load-block
  "Deserialize a Block from a serialized hex string (as returned by
  the RPC API)"
  [data-str]
  (let [data (util/hex-str-to-byte-arr data-str)
        reader (BinaryReader. (MemoryStream. data))
        block (Block.)]
    (.Deserialize block reader)
    (obj->clj block)))

(defn context-to-raw-tx
  "Create a transaction string that is accepted by RPC 'sendrawtransaction'"
  [ctx]
  (set! (.Scripts (.Verifiable ctx)) (.GetScripts ctx))
  (->> ctx .Verifiable (. Neo.IO.Helper ToArray) Helper/ToHexString))

(defn rpc-request
  "Make a request to a NEO RPC server"
  ([method] (rpc-request method []))
  ([method params]
   (let [rpc-url (rand-nth rpc-list)
         request (WebRequest/Create rpc-url)
         data (json/write-str {:jsonrpc "2.0" :id 1 :method method :params params})
         byte-data (.GetBytes Encoding/UTF8 data)]
    (set! (.Method request) "POST")
    (set! (.ContentType request) "application/x-www-form-urlencoded")
    (set! (.ContentLength request) (.Length byte-data))
    (let [stream (.GetRequestStream request)]
      (.Write stream byte-data 0 (.Length byte-data))
      (.Close stream)
      (let [response (.GetResponse request)
            stream-in (.GetResponseStream response)
            reader (StreamReader. stream-in)
            response-string (.ReadToEnd reader)]
        (.Close reader)
        (.Close stream-in)
        (.Close response)
        (get (json/read-str response-string) "result"))))))

(defn relay [ctx]
  (rpc-request "sendrawtransaction" [(context-to-raw-tx ctx)]))

(defn sync
  "Sync blockchain using RPC calls"
  ([bc] (sync bc true))
  ([bc verbose]
   (let [height (int (rpc-request "getblockcount"))]
     (loop [i (int (.Height bc))]
       (when (< i height)
         (when verbose (println (str "load block " i " of " height)))
         (->> (rpc-request "getblock" [i]) load-block :object (.AddBlock bc))
         (recur (inc i)))))))

(defn sync-thread
  "Keep blockchain synced in a background thread"
  [bc]
  (System.Threading.Thread.
   (gen-delegate
    System.Threading.ThreadStart []
    (while true
      (Thread/Sleep (* Blockchain/SecondsPerBlock 1000))
      (sync bc false)))))

(defn- persist-state [path]
  (spit (str path "/state.edn") @state))

(defn- restore-state! [path]
  (reset! state (read-string (slurp (str path "/state.edn")))))

(defn create
  ([] (create chain-path))
  ([path]
   (let [bc (LevelDBBlockchain. path)
         thread (sync-thread bc)]
     (set! (.VerifyBlocks bc) false)
     (when (zero? (.Height bc))
       (update-state (get-block bc 0))
       (persist-state path))
     (restore-state! path)
     (on-block-persist #((update-state %2)
                         (persist-state path)))
     (Blockchain/RegisterBlockchain bc)
     (.Start thread)
     bc)))

(defn monitored-run [script]
  (let [b Blockchain/Default
        accounts (.CreateCache b (type-args UInt160 AccountState))
        validators (.CreateCache b (type-args ECPoint ValidatorState))
        assets (.CreateCache b (type-args UInt256 AssetState))
        contracts (.CreateCache b (type-args UInt160 ContractState))
        storages (.CreateCache b (type-args StorageKey StorageItem))
        script-table (CachedScriptTable. contracts)
        service (StateMachine. accounts validators assets contracts storages)
        engine (ApplicationEngine. TriggerType/Application nil script-table service Fixed8/Zero true)]
    (doto engine
      (.LoadScript script false)
      .Execute)
    (let [created-contracts (get-private-field service "contracts_created")]
      {:engine engine
       :created-contracts created-contracts})))
