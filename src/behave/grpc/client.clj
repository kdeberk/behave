(ns behave.grpc.client
  (:import [behave.grpc.stream RequestResponseStreamGrpc]
           [io.grpc.stub StreamObserver]))

(defn channel []
  (behave.grpc.stream.RequestResponseStreamGrpc/newStub
   (-> (io.grpc.ManagedChannelBuilder/forAddress "127.0.0.1" (int 9090))
       .usePlaintext
       .build)))

(defn async-stub []
  (.createEndpoint (channel) (reify StreamObserver
                               (onNext [_ v] (print "Received a message"))
                               (onCompleted [_] (print "Completed"))
                               (onError [_ e] (println "Error" e)))))
