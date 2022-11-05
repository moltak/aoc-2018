(ns hello-iced.util)

(defn not-nil?
  [x]
  (not (nil? x)))

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

(defn print-interm
  [x]
  (println x) x)

(defn char->int [x] (int (.charAt x 0)))


(defn idle?
  [worker]
  (= "IDLE" (:status worker)))
