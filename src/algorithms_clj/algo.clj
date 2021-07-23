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
