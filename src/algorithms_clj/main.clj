(ns algorithms-clj.main
  (:gen-class)
  (:require [algorithms-clj.algo :as algo]))

(defn -main
  "main function"
  [& args]
  ;;(println "Hello, World!")

  ;; (->
  ;;  (algo/selection-sort ["c" "a" "e" "b" "f" "z"])
  ;;  (println))

  ;; (defn get-sequence []
  ;;   (let [list1 (transient []) sorted (sort ["a" "b" "c" "d" "e" "f" "g"])]
  ;;     (doseq [item1 sorted]
  ;;       (conj! list1 item1) )
  ;;     (persistent! list1) ) )

  ;; (def list1 (get-sequence))


  (->
   (algo/binary-search ["a" "b" "c" "d" "f" "g" "h" "i"] "e")
   (println))
  

  ;; (println list1)
  ;; (println (get list1 0))

  ;; (let [midAm (atom 0)
  ;;       loAm (atom 0) hiAm (atom (dec (count list1))) val1 "z"]
  ;;   (while (<= @loAm @hiAm)
  ;;     (reset! midAm (+ (int (/ (- @hiAm @loAm) 2)) @loAm))
  ;;     (println "----------------")
  ;;     (println "lo: " @loAm " hi: " @hiAm " mid: " @midAm)
  ;;     (println "lo: " (get list1 @loAm) " hi: " (get list1 @hiAm) " mid: " (get list1 @midAm))
  ;;     (println "----------------")
  ;;     (if (= (compare val1 (get list1 @midAm)) 0)
  ;;       (do (println "end with: " @midAm))
  ;;       (if (> (compare val1 (get list1 @midAm)) 0)
  ;;         (do (reset! loAm (inc @midAm)))
  ;;         (do (reset! hiAm (dec @midAm))) )
  ;;       )
  ;;     (Thread/sleep 1000)
  ;;     )
  ;;   )
  
  )
