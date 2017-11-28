(ns neo-clj.crypto
  (:import
   [System.IO BinaryReader MemoryStream File StreamReader]
   Org.BouncyCastle.Crypto.Digests.Sha256Digest
   Org.BouncyCastle.Crypto.EC.CustomNamedCurves
   [Org.BouncyCastle.Crypto.Parameters ECDomainParameters
    ECPrivateKeyParameters]
   [Org.BouncyCastle.Asn1 DerSequenceGenerator DerInteger]
   [Org.BouncyCastle.Crypto.Signers HMacDsaKCalculator ECDsaSigner]))

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
  "Sign a byte-array message with a private key from keypair. Message
  should be a hashed byte array."
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

