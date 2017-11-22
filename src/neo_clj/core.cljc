(assembly-load "Neo")
(assembly-load "BouncyCastle.Crypto")

(ns neo-clj.core
  (:import
   [System BitConverter Convert Array]
   [System.IO BinaryReader MemoryStream ]
   System.Text.Encoding
   Neo.Implementations.Wallets.EntityFramework.UserWallet
   Neo.Implementations.Blockchains.LevelDB.LevelDBBlockchain
   Neo.Cryptography.Crypto
   [Neo.Cryptography.ECC ECPoint ECCurve ECDsa]
   [Neo.Core IVerifiable Witness ContractTransaction TransactionOutput TransactionAttribute Blockchain CoinReference Transaction Block]
   [Neo.Network LocalNode]
   [Neo.SmartContract ContractParametersContext Contract]
   [Neo.Wallets Wallet VerificationContract Coin]
   [Neo Helper Fixed8 UInt256 Settings]
   Org.BouncyCastle.Crypto.Digests.Sha256Digest
   Org.BouncyCastle.Crypto.EC.CustomNamedCurves
   [Org.BouncyCastle.Crypto.Parameters ECDomainParameters ECPrivateKeyParameters]
   [Org.BouncyCastle.Asn1 DerSequenceGenerator DerInteger]
   [Org.BouncyCastle.Crypto.Signers HMacDsaKCalculator ECDsaSigner]))

(def settings Settings/Default)

(def chain-path "./Chain")

(def public-keys (.StandbyValidators settings))

(def asset-ids
  {:neo "c56f33fc6ecfcd0c225c4ab356fee59390af8560be0e930faebe74a6daff7c9b"
   :gas "602c79718b16e442de58778e148d0b1084e3b2dffd5de6b7b16cee7969282de7"})

(def wifs
  ["KxyjQ8eUa4FHt3Gvioyt1Wz29cTUrE4eTqX3yFSk1YFCsPL8uNsY"
   "KzfPUYDC9n2yf4fK5ro4C8KMcdeXtFuEnStycbZgX3GomiUsvX6W"
   "L2oEXKRAAMiPEZukwR5ho2S6SMeQLhcK9mF71ZnF7GvT8dU4Kkgz"
   "KzgWE3u3EDp13XPXXuTKZxeJ3Gi8Bsm8f9ijY3ZsCKKRvZUo1Cdn"])

(defn create-wallet []
  (UserWallet/Create "test.db3" "test"))

(defn open-wallet [file passw]
  (UserWallet/Open file passw))

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

(defn make-transaction [wallet to-address]
  (let [scripthash-to (Wallet/ToScriptHash to-address)
        fee (Fixed8/Zero)
        output (doto (TransactionOutput.)
                   (#(set! (.AssetId %) (UInt256/Parse (:neo asset-ids))))
                   (#(set! (.Value %) (Fixed8/Parse "100000000")))
                   (#(set! (.ScriptHash %) scripthash-to)))
        tx (doto (ContractTransaction.)
             (#(set! (.Outputs %) (into-array [output]))))
        txx (.MakeTransaction wallet tx nil fee)
        ctx (ContractParametersContext. txx)]
    ctx))

(defn to-der [sig]
  (let [[r s] (map #(Org.BouncyCastle.Math.BigInteger. %) sig)
        bos (MemoryStream. 64)
        seq (DerSequenceGenerator. bos)]
    (.AddObject seq (DerInteger. r))
    (.AddObject seq (DerInteger. s))
    (.Close seq)
    (.ToArray bos)))

(defn hash256
  "Calculate Sha256 for a byte array"
  [data]
  (let [digest (Sha256Digest.)
        output (make-array Byte 32)]
    (.BlockUpdate digest data 0 (count data))
    (.DoFinal digest output 0)
    output))

(defn hash256-digest [data]
  (let [digest (Sha256Digest.)]
    (.BlockUpdate digest data 0 (count data))
    digest))

(defn generate-signature [verifiable key-pair]
  "Create signature of a verifiable using private key from key-pair"
  (let [nist-p256 (ECCurve/Secp256r1)
        ecdsa (ECDsa. (.PrivateKey key-pair) nist-p256)
        [r s] (->> verifiable
                   (. Neo.Core.Helper GetHashData)  ;; message to byte-array
                   hash256                          ;; use sha256 hash of message
                   (.GenerateSignature ecdsa)       ;; generate [r,s] values
                   (map #(vec (.ToByteArray %)))    ;; convert to byte vector
                   (map #(subvec % 0 32))           ;; use 32-bytes
                   ;; (map reverse)                    ;; to big-endian        
                   )]
    (into-array (concat r s))))

(defn reverse-b [byte-arr]
  (into-array (reverse (vec byte-arr))))

(defn to-signature [[r s]]
  (->> [r s]
       (map #(.ToByteArrayUnsigned %))
       (apply concat)
       into-array))

(defn sign [message key-pair]
  (let [curve (CustomNamedCurves/GetByName "secp256r1")
        domain-param (ECDomainParameters. (.Curve curve) (.G curve) (.N curve) (.H curve))
        bigi (Org.BouncyCastle.Math.BigInteger. (int 1) (.PrivateKey key-pair))
        private-key (ECPrivateKeyParameters. "ECDSA"
                                             bigi
                                             domain-param)
        digest (hash256-digest message)
        signer (ECDsaSigner. (HMacDsaKCalculator. digest))] ;; deterministic signer
    (.Init signer true private-key)
    (to-signature
     (.GenerateSignature signer message))))

(defn sign-context [wallet ctx]
  "Sign a context using the necessary keys from wallet. Returns
  boolean wether all scripthashes could be signed."
  (let [result 
        (map
         (fn [script-hash]
           (when-let [contract (.GetContract wallet script-hash)]
             (when-let [key (.GetKeyByScriptHash wallet script-hash)]
               ;; (let [signature (generate-signature (.Verifiable ctx) key)]
               (let [message (->> (.Verifiable ctx) (. Neo.Core.Helper GetHashData) hash256)
                     signature (sign  message key)]
                 (.AddSignature ctx contract (.PublicKey key) signature)))))
         (.ScriptHashes ctx))]
    (every? identity result)))

(defn context-to-raw-tx [ctx]
  (set! (.Scripts (.Verifiable ctx)) (.GetScripts ctx))
  (->> ctx .Verifiable (. Neo.IO.Helper ToArray) Helper/ToHexString))


;; Example: claiming initial NEO
(def block1 (load-block "000000000000000000000000000000000000000000000000000000000000000000000000845c34e7c1aed302b1718e914da0c42bf47c476ac4d89671f278d8ab6d27aa3d65fc8857000000001dac2b7c00000000be48d3a3f5d10013ab9ffee489706078714f1ea2010001510400001dac2b7c00000000400000455b7b226c616e67223a227a682d434e222c226e616d65223a22e5b08fe89a81e882a1227d2c7b226c616e67223a22656e222c226e616d65223a22416e745368617265227d5d0000c16ff28623000000da1745e9b549bd0bfa1a569971c77eba30cd5a4b00000000400001445b7b226c616e67223a227a682d434e222c226e616d65223a22e5b08fe89a81e5b881227d2c7b226c616e67223a22656e222c226e616d65223a22416e74436f696e227d5d0000c16ff286230008009f7fd096d37ed2c0e3f7f0cfc924beef4ffceb680000000001000000019b7cffdaa674beae0f930ebe6085af9093e5fe56b34a5c220ccdcf6efc336fc50000c16ff2862300be48d3a3f5d10013ab9ffee489706078714f1ea201000151"))

(create-blockchain!)
(def bc Blockchain/Default)
(.AddBlock Blockchain/Default block1)

(def wallets
  (doall (map-indexed (fn [i w]
                        (let [wal (UserWallet/Create (str "wal" i) "test")]
                          (.Import wal w) wal)) wifs)))
(def w (first wallets))

(def to-address (-> public-keys first pub-hash-to-ecpoint
                    VerificationContract/CreateSignatureContract
                    (.Address)))

(dorun (map add-multi-sig-contract wallets))
(dorun (map #(.Rebuild %) wallets))
(def ctx (make-transaction w to-address))

;; (dorun (map #(sign-context (nth wallets %) ctx) [1 2 3]))

