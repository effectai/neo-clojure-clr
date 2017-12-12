(ns neo-clj.rpc
  (:require
   [neo-clj.blockchain :as blockchain]
   [neo-clj.wallet :as wallet]
   [neo-clj.core :refer [claim-initial-neo-tx]]
   [clojure.data.json :as json]
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]])
  (:import
   Neo.Network.RPC.RpcServer
   Neo.Network.LocalNode
   Neo.Core.Blockchain
   [Neo.IO.Json JArray JObject]))

(def default-settings {:port 10332 :protocol "http"})

(def method-needs-blockchain? {"getblock" true "getassets" true})

(def state (atom {:wallet nil}))

(defn- process-gas-claim [params]
  (let [wallet (:wallet @state)
        claim-ctx
        (try (if (empty? params)
               (wallet/claim-gas-tx (:wallet @state))
               (wallet/claim-gas-tx (:wallet @state) (first params)))
             (catch Exception e nil))]
    (if (nil? claim-ctx)
      "no claimable gas"
      (do
        (wallet/sign wallet claim-ctx)
        (blockchain/relay claim-ctx)
        "success"))))

(defn- process-message [settings method params]
  (if (and (not Blockchain/Default) (method-needs-blockchain? method))
    {:error "No blockchain loaded"}
    (case method
      "getversion" {:port (:port settings) :useragent "NEO-CLJ:0.0.0"}
      "getassets" (:assets @blockchain/state)
      "getkeys" (wallet/get-keys (:wallet @state))
      "makekeys" (let [num (if (empty? params) 1 (first params))]
                   (dorun (dotimes [_ num] (.CreateKey (:wallet @state))))
                   "success")
      "claiminitialneo" (do (-> (:wallet @state)
                                wallet/get-keys
                                first :address
                                claim-initial-neo-tx
                                :ctx blockchain/relay)
                            "success")
      "claimgas" (process-gas-claim params)
      nil)))

(defn create-server
  "Initialize an extended RPC server. Can be started with 'start-server'"
  ([] (create-server {}))
  ([settings]
   (let [{:keys [port protocol] :as settings} (merge default-settings settings)
         uri-prefix (str protocol "://*:" port)
         server
         (proxy [RpcServer] [nil]
           (Process [method params]
             (let [clj-params (map #(.get_Value %) params)
                   response (process-message settings method clj-params)]
               (if (nil? response)
                 (proxy-super Process method params)
                 (JObject/Parse (json/write-str response))))))]
     (-> settings
         (assoc-in [:server] server)))))

(defn start-server
  [{:keys [server protocol port]}]
  (println "Starting extended RPC server on port" port)

  (let [w (wallet/open-or-create "wallet.db3" "duplo")]
    (swap! state #(assoc-in % [:wallet] w)))

  (.Start server (into-array [(str protocol "://*:" port)])))
