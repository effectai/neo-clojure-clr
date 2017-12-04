(ns neo-clj.wallet
  (:require
   [neo-clj.blockchain :as blockchain]
   [neo-clj.crypto :as crypto])
  (:import
   [System.IO File Directory]
   [Neo Fixed8 Helper]
   Neo.Implementations.Wallets.EntityFramework.UserWallet
   [Neo.Core TransactionOutput ContractTransaction Blockchain
    ClaimTransaction TransactionAttribute CoinReference CoinState]
   [Neo.Cryptography.ECC ECCurve ECPoint]
   [Neo.Wallets Wallet VerificationContract Coin]
   [Neo.SmartContract ContractParametersContext Contract]))

;; The wallets corresponding to the standby validators from privnet
(def wifs
  ["KxyjQ8eUa4FHt3Gvioyt1Wz29cTUrE4eTqX3yFSk1YFCsPL8uNsY"
   "KzfPUYDC9n2yf4fK5ro4C8KMcdeXtFuEnStycbZgX3GomiUsvX6W"
   "L2oEXKRAAMiPEZukwR5ho2S6SMeQLhcK9mF71ZnF7GvT8dU4Kkgz"
   "KzgWE3u3EDp13XPXXuTKZxeJ3Gi8Bsm8f9ijY3ZsCKKRvZUo1Cdn"])

(defn create [path passw]
  (UserWallet/Create path passw))

(defn open [path passw]
  (UserWallet/Open path passw))

(defn open-or-create
  "Open a wallet relative to the current directory, or create it if it
  doesn't exist"
  [path passw]
  (let [path (str (Directory/GetCurrentDirectory) "/" path)]
    (if (File/Exists path)
      (open path passw)
      (create path passw))))

(defn neo-balance [wallet]
  (->> (:neo blockchain/asset-ids) (.GetAvailable wallet)))

(defn gas-balance [wallet]
  (->> (:gas blockchain/asset-ids) (.GetAvailable wallet)))

(defn balance-for-key [wallet script-hash]
  (let [coins (->> wallet .GetCoins vec
                   (filter #(= (.. % Output ScriptHash) script-hash))
                   (filter #(.HasFlag (.State %) CoinState/Confirmed))
                   (filter #(not (.HasFlag (.State %) CoinState/Spent)))
                   (filter #(not (.HasFlag (.State %) CoinState/Frozen)))
                   (filter #(not (.HasFlag (.State %) CoinState/Locked)))
                   (filter #(not (.HasFlag (.State %) CoinState/WatchOnly))))
        neo-coins (filter #(= (.. % Output AssetId) (:neo blockchain/asset-ids)) coins)
        gas-coins (filter #(= (.. % Output AssetId) (:gas blockchain/asset-ids)) coins)]
    {:neo (reduce #(+ %1 (int (str (.. %2 Output Value)))) 0 neo-coins)
     :gas (reduce #(+ %1 (int (str (.. %2 Output Value)))) 0 gas-coins)}))

(defn get-a-public-key
  "Generate new keypair for the wallet and return the public"
  [wallet]
  (-> wallet .CreateKey .PublicKey .ToString))

(defn match-key
  "Get a KeyPair from wallet that matches a public key"
  [key wallet]
  (->> (.EncodePoint key true)
       Neo.Core.Helper/ToScriptHash
       (. wallet GetKey)))

(defn pub-hash-to-ecpoint
  "Convert public-key hash to ECPoint"
  [hash]
  (->> hash
       Helper/HexToBytes
       (#(. ECPoint DecodePoint % ECCurve/Secp256r1))))

(defn add-multi-sig-contract [wallet public-keys]
  (let [keys (map pub-hash-to-ecpoint public-keys)
        key (->> (map #(match-key % wallet) keys)
                 (filter #(not (nil? %)))
                 first)
        contract (VerificationContract/CreateMultiSigContract (.PublicKeyHash key) 3 (into-array keys))]
    (.AddContract wallet contract)
    contract))

(defn make-transaction
  ([wallet to-address amount] (make-transaction wallet to-address amount (:neo blockchain/asset-ids)))
  ([wallet to-address amount asset-id]
  (let [scripthash-to (Wallet/ToScriptHash to-address)
        f8-amount (Fixed8/Parse amount)
        fee (Fixed8/Zero)
        output (doto (TransactionOutput.)
                   (#(set! (.AssetId %) asset-id))
                   (#(set! (.Value %) f8-amount))
                   (#(set! (.ScriptHash %) scripthash-to)))
        tx (doto (ContractTransaction.)
             (#(set! (.Outputs %) (into-array [output]))))
        txx (.MakeTransaction wallet tx nil fee)
        ctx (ContractParametersContext. txx)]
    {:tx tx
     :ctx ctx})))

(defn sign-context
  "Sign a context using the necessary keys from wallet. Returns
  boolean wether all scripthashes could be signed."
  [wallet ctx]
  (let [result
        (map
         (fn [script-hash]
           (when-let [contract (.GetContract wallet script-hash)]
             (when-let [key (.GetKeyByScriptHash wallet script-hash)]
               (let [message (->> (.Verifiable ctx) (. Neo.Core.Helper GetHashData) crypto/hash256)
                     signature (crypto/sign  message key)]
                 (.AddSignature ctx contract (.PublicKey key) signature)))))
         (.ScriptHashes ctx))]
    (every? identity result)))

(defn pub-key-to-address [pub-hash]
  (-> pub-hash pub-hash-to-ecpoint
      VerificationContract/CreateSignatureContract
      (.Address)))

(defn claim-gas-tx [wallet]
  (let [unclaimed-coins (.GetUnclaimedCoins wallet)]
    (when (empty? unclaimed-coins)
      (throw (Exception. "No claimable gas")))
    (let [claims (map #(.Reference %) unclaimed-coins)
          asset-id (.Hash Blockchain/UtilityToken)
          output (doto (TransactionOutput.)
                   (#(set! (.AssetId %) asset-id))
                   (#(set! (.Value %) (. Blockchain CalculateBonus claims false)))
                   (#(set! (.ScriptHash %) (.GetChangeAddress wallet))))
          tx (doto (ClaimTransaction.)
               (#(set! (.Claims %) (into-array claims)))
               (#(set! (.Attributes %) (make-array
                                        TransactionAttribute 0)))
               (#(set! (.Inputs %) (make-array CoinReference 0)))
               (#(set! (.Outputs %) (into-array [output]))))
          ctx (ContractParametersContext. tx)]
      ctx)))

(defn get-keys
  "Get clojure map of info about all the keys in wallet"
  [wallet]
  (letfn [(addr [k]
            (let [address (-> k .PublicKey str pub-key-to-address)
                  script-hash (Wallet/ToScriptHash address)]
              {:public-key (-> k .PublicKey str)
               :private-key (-> k .PrivateKey Helper/ToHexString)
               :wif (.Export k)
               :balance (balance-for-key wallet script-hash)
               :script-hash (str script-hash)
               :address address}))]
    (->> wallet .GetKeys vec (map addr))))

