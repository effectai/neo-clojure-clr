(ns neo-clj.wallet
  (:require
   [neo-clj.blockchain :as blockchain]
   [neo-clj.crypto :as crypto])
  (:import
   [Neo Fixed8 Helper]
   Neo.Implementations.Wallets.EntityFramework.UserWallet
   [Neo.Core TransactionOutput ContractTransaction Blockchain
    ClaimTransaction TransactionAttribute CoinReference]
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

(defn neo-balance [wallet]
  (->> (:neo blockchain/asset-ids) (.GetAvailable wallet)))

(defn gas-balance [wallet]
  (->> (:gas blockchain/asset-ids) (.GetAvailable wallet)))

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

