(assembly-load "Neo")
(assembly-load "BouncyCastle.Crypto")
(assembly-load "deps/nuget/clojure.data.json.0.2.1.0/lib/net35/clojure.data.json.dll")

(ns neo-clj.core
  (:require [clojure.data.json :as json])
  (:import
   System.Net.WebRequest
   System.Threading.Monitor
   [System BitConverter Convert Array]
   [System.IO BinaryReader MemoryStream File StreamReader]
   System.Text.Encoding
   Neo.Implementations.Wallets.EntityFramework.UserWallet
   Neo.Implementations.Blockchains.LevelDB.LevelDBBlockchain
   Neo.Cryptography.Crypto
   [Neo.Cryptography.ECC ECPoint ECCurve ECDsa]
   [Neo.Core IVerifiable Witness ContractTransaction TransactionOutput
    ClaimTransaction TransactionAttribute Blockchain CoinReference
    Transaction Block InvocationTransaction StorageKey StorageItem]
   [Neo.VM ScriptBuilder VMState ExecutionEngine]
   [Neo.Network LocalNode]
   [Neo.SmartContract ContractParametersContext Contract
   ContractParameterType ApplicationEngine]
   [Neo.Wallets Wallet VerificationContract Coin]
   [Neo Helper Fixed8 UInt256 Settings]
   Org.BouncyCastle.Crypto.Digests.Sha256Digest
   Org.BouncyCastle.Crypto.EC.CustomNamedCurves
   [Org.BouncyCastle.Crypto.Parameters ECDomainParameters
    ECPrivateKeyParameters]
   [Org.BouncyCastle.Asn1 DerSequenceGenerator DerInteger]
   [Org.BouncyCastle.Crypto.Signers HMacDsaKCalculator ECDsaSigner]))

(def settings Settings/Default)

(def chain-path "./Chain")

(def public-keys (.StandbyValidators settings))

(def asset-ids
  {:neo "c56f33fc6ecfcd0c225c4ab356fee59390af8560be0e930faebe74a6daff7c9b"
   :gas "602c79718b16e442de58778e148d0b1084e3b2dffd5de6b7b16cee7969282de7"})

;; The wallets corresponding to the standby validators from privnet
(def wifs
  ["KxyjQ8eUa4FHt3Gvioyt1Wz29cTUrE4eTqX3yFSk1YFCsPL8uNsY"
   "KzfPUYDC9n2yf4fK5ro4C8KMcdeXtFuEnStycbZgX3GomiUsvX6W"
   "L2oEXKRAAMiPEZukwR5ho2S6SMeQLhcK9mF71ZnF7GvT8dU4Kkgz"
   "KzgWE3u3EDp13XPXXuTKZxeJ3Gi8Bsm8f9ijY3ZsCKKRvZUo1Cdn"])

(defn create-wallet [path passw]
  (UserWallet/Create path passw))

(defn open-wallet [file passw]
  (UserWallet/Open file passw))

(defn neo-balance [wallet]
  (->> (:neo asset-ids) UInt256/Parse (.GetAvailable wallet)))

(defn gas-balance [wallet]
  (->> (:gas asset-ids) UInt256/Parse (.GetAvailable wallet)))

(defn get-a-public-key
  "Generate new keypair for the wallet and return the public"
  [wallet]
  (-> wallet .CreateKey .PublicKey .ToString))

(defn match-key-to-wallet
  "Get a KeyPair from wallet that matches a public key"
  [key wallet]
  (->> key
       (#(.EncodePoint % true))
       (. Neo.Core.Helper ToScriptHash)
       (. wallet GetKey)))

(defn pub-hash-to-ecpoint
  "Convert public-key hash to ECPoint"
  [hash]
  (->> hash
       (. Helper HexToBytes)
       (#(. ECPoint DecodePoint % ECCurve/Secp256r1))))

(defn hex-str-to-byte-arr
  "Utility function to convert a hex-decimal string to a byte array"
  [hex]
  (let [len (.Length hex)
        ba (make-array Byte (/ len 2))]
    (loop [i 0 arr ba]
      (if (< i len)
        (recur (+ i 2)
               (do
                 (aset-byte arr (/ i 2)
                            (Convert/ToByte (.Substring hex i 2) 16))
                 arr))
        arr))))
  
(defn add-multi-sig-contract [wallet]
  (let [keys (map pub-hash-to-ecpoint public-keys)
        key (->> (map #(match-key-to-wallet % wallet) keys)
                 (filter #(not (nil? %)))
                 first)
        contract (VerificationContract/CreateMultiSigContract (.PublicKeyHash key) 3 (into-array keys))]
    (.AddContract wallet contract)
    contract))

(defn create-blockchain! []
  (Blockchain/RegisterBlockchain (LevelDBBlockchain. chain-path))
  (set! (.VerifyBlocks Blockchain/Default) false))

(defn load-block
  "Deserialize a Block from a serialized hex string (as returned by
  the RPC API)"
  [data-str]
  (let [data (hex-str-to-byte-arr data-str)
        reader (BinaryReader. (MemoryStream. data))
        block (Block.)]
    (.Deserialize block reader)
    block))

(defn make-transaction [wallet to-address amount]
  (let [scripthash-to (Wallet/ToScriptHash to-address)
        fee (Fixed8/Zero)
        output (doto (TransactionOutput.)
                   (#(set! (.AssetId %) (UInt256/Parse (:neo asset-ids))))
                   (#(set! (.Value %) (Fixed8/Parse amount)))
                   (#(set! (.ScriptHash %) scripthash-to)))
        tx (doto (ContractTransaction.)
             (#(set! (.Outputs %) (into-array [output]))))
        txx (.MakeTransaction wallet tx nil fee)
        ctx (ContractParametersContext. txx)]
    ctx))

(defn to-der
  "Convert an [r, s] signature to array in DER format"
  [sig]
  (let [[r s] (map #(Org.BouncyCastle.Math.BigInteger. %) sig)
        bos (MemoryStream. 64)
        seq (DerSequenceGenerator. bos)]
    (.AddObject seq (DerInteger. r))
    (.AddObject seq (DerInteger. s))
    (.Close seq)
    (.ToArray bos)))

(defn to-signature
  "Convert [r, s] signature to array by concatenating"
  [[r s]]
  (->> [r s]
       (map #(.ToByteArrayUnsigned %))
       (apply concat)
       into-array))

(defn hash256-digest [data]
  (let [digest (Sha256Digest.)]
    (.BlockUpdate digest data 0 (count data))
    digest))

(defn hash256
  "Calculate Sha256 hash for a byte array"
  [data]
  (let [digest (hash256-digest data)
        output (make-array Byte 32)]
    (.DoFinal digest output 0)
    output))

(defn reverse-b [byte-arr]
  (into-array (reverse (vec byte-arr))))

(defn sign
  "Sign a byte-array message with a keypair. Message should be a
  hashed byte array."
  [message key-pair]
  (let [curve (CustomNamedCurves/GetByName "secp256r1")
        domain-param (ECDomainParameters. (.Curve curve) (.G curve) (.N curve) (.H curve))
        bigi (Org.BouncyCastle.Math.BigInteger. (int 1) (.PrivateKey key-pair))
        private-key (ECPrivateKeyParameters. "ECDSA" bigi domain-param)
        digest (hash256-digest message)
        signer (ECDsaSigner. (HMacDsaKCalculator. digest))]  ; deterministic signer
    (.Init signer true private-key)
    (to-signature
     (.GenerateSignature signer message))))

(defn sign-context
  "Sign a context using the necessary keys from wallet. Returns
  boolean wether all scripthashes could be signed."
  [wallet ctx]
  (let [result 
        (map
         (fn [script-hash]
           (when-let [contract (.GetContract wallet script-hash)]
             (when-let [key (.GetKeyByScriptHash wallet script-hash)]
               (let [message (->> (.Verifiable ctx) (. Neo.Core.Helper GetHashData) hash256)
                     signature (sign  message key)]
                 (.AddSignature ctx contract (.PublicKey key) signature)))))
         (.ScriptHashes ctx))]
    (every? identity result)))

(defn context-to-raw-tx
  "Create a transaction string that is accepted by RPC 'sendrawtransaction'"
  [ctx]
  (set! (.Scripts (.Verifiable ctx)) (.GetScripts ctx))
  (->> ctx .Verifiable (. Neo.IO.Helper ToArray) Helper/ToHexString))

(defn pub-key-to-address [pub-hash]
  (-> pub-hash pub-hash-to-ecpoint
      VerificationContract/CreateSignatureContract
      (.Address)))

(defn claim-initial-neo
  "Create a signed raw transaction that claims the NEO from genesis block"
  [address]
  (let [to-address (pub-key-to-address address)
        wallets (map-indexed
                 #(let [wal (create-wallet (str "wal" %1) "test")]
                    (.Import wal %2) wal) wifs)]
    (dorun (map add-multi-sig-contract wallets))      ; add the multi-sig tx
    (-> wallets first (.Rebuild))                     ; rebuild wallet balances
    (while (-> wallets first (.GetCoins) empty?) -1)  ; wait for rebuild...
    (let [ctx (make-transaction
               (first wallets) to-address "100000000")
          signs (dorun (map #(sign-context % ctx) wallets))]
      (if (not (.Completed ctx))
        (throw (Exception. "Failed to sign the transaction"))
        {:tx (.Verifiable ctx)
         :raw-tx (context-to-raw-tx ctx)}))))


(defn deploy-contract-tx
  [wallet avm-file param-list return-type]
  (let [script (File/ReadAllBytes avm-file)
        sb (ScriptBuilder.)
        type ^ContractParameterType (first (Helper/HexToBytes return-type))
        need-storage true]
    (doto sb
      (.EmitPush "description")
      (.EmitPush "jesse@effect.ai")
      (.EmitPush "Effect")
      (.EmitPush "0.1")
      (.EmitPush "Name")
      (.EmitPush need-storage)
      (.EmitPush type)
      (.EmitPush (Helper/HexToBytes param-list))
      (.EmitPush script)
      (.EmitSysCall "Neo.Contract.Create"))
    (let [tx (InvocationTransaction.)]
      (set! (.Script tx) (.ToArray sb))
      {:tx tx
       :ctx (-> tx
                (#(.MakeTransaction wallet % nil (Fixed8/Zero)))
               (ContractParametersContext.))
       :script-hash (Neo.Core.Helper/ToScriptHash script)})))

(defn invoke-contract-tx [wallet to-address script-hash]
  (let [scripthash-to (.ToArray (Wallet/ToScriptHash to-address))
        sb (ScriptBuilder.)]
    (doto sb
      (.EmitPush (make-array Byte 0))
      (.EmitPush (make-array Byte 0))
      (.EmitPush "Deploy")
      (.EmitPush scripthash-to)
      (.EmitAppCall (.ToArray script-hash) false))
    (let [tx (InvocationTransaction.)]
      (set! (.Script tx) (.ToArray sb))
      (-> tx
          (#(.MakeTransaction wallet % nil (Fixed8/Zero)))
          (ContractParametersContext.)))))

(def request-map {:jsonrpc "2.0" :id 1})
(def rpc-url "http://localhost:10332")
(defn rpc-request
  "Make a request to a NEO RPC server"
  ([method] (rpc-request method []))
  ([method params]
   (let [request (WebRequest/Create rpc-url)
         data (-> request-map
                  (assoc-in [:method] method)
                  (assoc-in [:params] params)
                  json/write-str)
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

(defn relay [raw-tx]
  (rpc-request "sendrawtransaction" [raw-tx]))

;; Initialize the blockchain
(create-blockchain!)
(.AddBlock Blockchain/Default Blockchain/GenesisBlock)
