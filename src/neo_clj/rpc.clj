(ns neo-clj.rpc
  (:require
   [neo-clj.blockchain :as blockchain]
   [clojure.data.json :as json]
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]])
  (:import
   Neo.Network.RPC.RpcServer
   Neo.Network.LocalNode
   Neo.Core.Blockchain
   [Neo.IO.Json JArray JObject]))

(def default-settings {:port 10332 :protocol "http"})

(def method-needs-blockchain? {"getblock" true})

(defn- process-message [settings method params]
  (if (and (not Blockchain/Default) (method-needs-blockchain? method))
    {:error "No blockchain loaded"}
    (case method
      "getversion" {:port (:port settings) :useragent "NEO-CLJ:0.0.0"}
      "getassets" (:assets @blockchain/state)
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
             (let [response (process-message settings method params)]
               (if (nil? response)
                 (proxy-super Process method params)
                 (JObject/Parse (json/write-str response))))))]
     (-> settings
         (assoc-in [:server] server)))))

(defn start-server
  [{:keys [server protocol port]}]
  (println "Starting extended RPC server on port" port)
  (.Start server (into-array [(str protocol "://*:" port)])))
