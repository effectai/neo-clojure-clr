(ns neo-clj.blockchain
  (:require
   [neo-clj.util :as util]
   [clojure.data.json :as json])
  (:import
   System.Net.WebRequest
   System.Text.Encoding
   System.Threading.Monitor
   [System.IO BinaryReader MemoryStream File StreamReader]
   [Neo Helper UInt256]
   [Neo.Core Blockchain Block]
   Neo.Implementations.Blockchains.LevelDB.LevelDBBlockchain
   [Neo.Network LocalNode]
   ))

(def chain-path "./Chain")


;; List of RPC servers used for syncing
(def rpc-list ["http://localhost:10332"])

(def utility-token Blockchain/UtilityToken)

(def asset-ids
  {:neo (UInt256/Parse "c56f33fc6ecfcd0c225c4ab356fee59390af8560be0e930faebe74a6daff7c9b")
   :gas (UInt256/Parse "602c79718b16e442de58778e148d0b1084e3b2dffd5de6b7b16cee7969282de7")})

(defn create! []
  (Blockchain/RegisterBlockchain (LevelDBBlockchain. chain-path))
  (set! (.VerifyBlocks Blockchain/Default) false)
  true)

(defn load-block
  "Deserialize a Block from a serialized hex string (as returned by
  the RPC API)"
  [data-str]
  (let [data (util/hex-str-to-byte-arr data-str)
        reader (BinaryReader. (MemoryStream. data))
        block (Block.)]
    (.Deserialize block reader)
    block))

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
  []
  (let [bc Blockchain/Default
        height (rpc-request "getblockcount")]
    (loop [i (int (.Height bc))]
      (when (< i height)
        (println (str "load block " i " of " height))
        (->> (rpc-request "getblock" [i]) load-block (.AddBlock bc))
        (recur (inc i))))
    height))

(defn sync-loop
  "Keep blockchain synced in a background thread"
  []
  (let [thread (doto (System.Threading.Thread.
                      (gen-delegate
                       System.Threading.ThreadStart []
                       )))]
    (throw (Exception. "Not implemented"))))

