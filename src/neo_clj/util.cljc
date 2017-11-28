(ns neo-clj.util
  (:import System.Convert))

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
