(ns algorithms-clj.algo
  (:gen-class))

(defn selection-sort [list1]

  (let [t1 (transient list1) minIdx (atom 0) tempVal (atom nil)]
    (doseq [idx1 (range 0 (count list1))]
      (reset! minIdx idx1)
      (doseq [idx2 (range (inc idx1) (count list1))]
        (when (> (compare (get t1 @minIdx) (get t1 idx2)) 0)
          (reset! minIdx idx2) )
        )
      (when (not (= @minIdx idx1))  ;; Exchange values
        (reset! tempVal (get t1 idx1))
        (assoc! t1 idx1 (get t1 @minIdx))
        (assoc! t1 @minIdx @tempVal) )
      
      )
    (persistent! t1) ) )

(defn binary-search [coll item]

  (let [midAm (atom 0)
        loAm (atom 0) hiAm (atom (dec (count coll))) resultAm (atom nil) val1 item]
    (while (and (<= @loAm @hiAm) (nil? @resultAm))
      (reset! midAm (+ (int (/ (- @hiAm @loAm) 2)) @loAm))
      ;; (println "----------------")
      ;; (println "lo: " @loAm " hi: " @hiAm " mid: " @midAm)
      ;; (println "lo: " (get coll @loAm) " hi: " (get coll @hiAm) " mid: " (get coll @midAm))
      ;; (println "----------------")
      (if (= (compare val1 (get coll @midAm)) 0)
        (do (reset! resultAm @midAm))
        (if (> (compare val1 (get coll @midAm)) 0)
          (do (reset! loAm (inc @midAm)))
          (do (reset! hiAm (dec @midAm))) )
        )
      )
    @midAm )
  )



(def RED true)
(def BLACK false)
(defrecord RBNode [key val nodeNumAm colorAm leftAm rightAm])


(defn rbtree-node-size [node]
  "Calculate tree size/tree weight."
  (let [cntAm (atom 0)]
    (defn -count-node-size [n]
      (when-not (or (nil? (:leftAm n)) (nil? @(:leftAm n)))
        (reset! cntAm (inc @cntAm))
        (-count-node-size @(:leftAm n)) )
      (when-not (or (nil? (:rightAm n)) (nil? @(:rightAm n)))
        (reset! cntAm (inc @cntAm))
        (-count-node-size @(:rightAm n)) ) )
    (-count-node-size node)
    (when-not (nil? node) (reset! cntAm (inc @cntAm)))
    (deref cntAm) ) )

(defn rbtree-rotate-right [node]
  "Rotate right."
  (let [x @(:leftAm node)]
    (reset! (:leftAm node) @(:right x))
    (reset! (:rightAm x) node)
    (reset! (:colorAm x) @(:colorAm node))
    (reset! (:colorAm node) RED)
    (reset! (:nodeNumAm x) @(:nodeNumAm node))
    (reset! (:nodeNumAm h) (+ 1 (rbtree-node-size @(:leftAm node))
                              (rbtree-node-size @(:rightAm node))))
    x ) )
