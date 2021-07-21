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
