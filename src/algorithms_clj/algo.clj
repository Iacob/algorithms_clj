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
    (reset! (:nodeNumAm node) (+ 1 (rbtree-node-size @(:leftAm node))
                              (rbtree-node-size @(:rightAm node))))
    x ) )



(defrecord ShortPath [from to lenAm prevAm])

(defn shortest-path [paths]

  (let [PathList (transient [])]
  
    (defn -merge-path [shortPath]
      (let [ifMatchedAm (atom false)]
        (doseq [pathItemIdx (range 0 (count PathList))]
          (let [pathItem (get PathList pathItemIdx)]
            
            (when (and (= (:from pathItem) (:from shortPath))
                       (= (:to pathItem) (:to shortPath)))
              
              (reset! ifMatchedAm true)
              
              (let [pathLen @(:lenAm pathItem) newLen @(:lenAm shortPath)]
                (when (or (nil? pathLen) (> pathLen newLen))
                  (reset! (:lenAm pathItem) newLen)
                  (reset! (:prevAm pathItem) @(:prevAm shortPath)) ) ) ) ) )
        (when-not @ifMatchedAm
          (conj! PathList shortPath) ) ) )
    
    
    (doseq [path paths]
      
      (let [newPath (->ShortPath (nth path 0)
                                 (nth path 1)
                                 (atom (nth path 2))
                                 (atom nil))]
        (conj! PathList newPath)
        
        (let [mergeList (transient [])]
          (doseq [pathItemIdx (range 0 (count PathList))]
            (let [pathItem (get PathList pathItemIdx)]
              (when (= (:to pathItem) (nth path 0))
                (conj! mergeList (->ShortPath (:from pathItem)
                                              (nth path 1)
                                              (atom (+ @(:lenAm pathItem)
                                                       (nth path 2)))
                                              (atom (:to pathItem)))) ) ) )
          (doseq [mergeItem (persistent! mergeList)]
            (-merge-path mergeItem) ) ) ) )

    (persistent! PathList) ) )



(defn threeway-sort [strCol]
  (defn -exch [arr pos1 pos2]
    (do
      (let [tmp (get arr pos1)]
        (assoc! arr pos1 (get arr pos2))
        (assoc! arr pos2 tmp) ) ) )
  (defn -sort1 [strArr lo hi charIdx]
    (when (< lo hi)
      (let [ltAm (atom lo) gtAm (atom hi)
            vAm (atom (get (get strArr lo) charIdx)) iAm (atom (inc lo))
            ]
        ;;
        (while (<= @iAm @gtAm)
          (let [t (get (get strArr @iAm) charIdx)]
            (if (< (int t) (int @vAm))
              (do
                (-exch strArr @ltAm @iAm)
                (swap! ltAm inc)
                (swap! iAm inc) )
              
              (if (> (int t) (int @vAm))
                (do
                  (-exch strArr @iAm @gtAm)
                  (swap! gtAm dec) )
                (swap! iAm inc) )
              )
            )
          )
        
        (-sort1 strArr lo (dec @ltAm) charIdx)
        (when (> (int @vAm) 0) (-sort1 strArr @ltAm @gtAm (inc charIdx)))
        (-sort1 strArr (inc @gtAm) hi charIdx) ) ) )
  
  (let [strArr (transient strCol)]
    (-sort1 strArr 0 (dec (count strCol)) 0)
    (println (persistent! strArr)) ) )
