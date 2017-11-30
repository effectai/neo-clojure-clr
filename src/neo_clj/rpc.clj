(ns neo-clj.rpc
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]])
  (:import
   Neo.Network.RPC.RpcServer
   Neo.Network.LocalNode
   [Neo.IO.Json JArray JObject]))

(def default-settings {:port 10332 :protocol "http"})

(defn create-server
  "Initialize an extended RPC server. Can be started with 'start-server'"
  ([] (create-server {}))
  ([settings]
   (let [{:keys [port protocol] :as settings} (merge default-settings settings)
         uri-prefix (str protocol "://*:" port)
         server
         (proxy [RpcServer] [nil]
           (Process [method params]
             (case method
               "getversion" (JObject/Parse (str "{\"port\": " port "}"))
               (proxy-super Process method params))))]
     (-> settings
         (assoc-in [:server] server)))))

(defn start-server
  [{:keys [server protocol port]}]
  (println "Starting extended RPC server on port" port)
  (.Start server (into-array [(str protocol "://*:" port)])))
