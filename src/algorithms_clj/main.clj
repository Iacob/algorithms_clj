(ns algorithms-clj.main
  (:gen-class)
  (:require [algorithms-clj.algo :as algo]))

(defn -main
  "main function"
  [& args]
  ;;(println "Hello, World!")

  (def paths [[:A :B 1] [:A :F 1]
              [:B :C 2] [:B :F 2]
              [:C :D 3] [:C :F 1]
              [:D :E 4]
              [:F :G 10]
              [:E :Z 5]
              [:G :Z 11]])

  ;;(def PathList (transient []))

  ;;(defrecord ShortPath [from to lenAm prevAm])


  (->
   (algo/shortest-path paths)
   (println))


  ;; (doseq [pathItem (persistent! PathList)]
  ;;   (println (:from pathItem) (:to pathItem) @(:lenAm pathItem) @(:prevAm pathItem)) )
  

  ;; (let [node12 (->Node "a" 1 (atom nil) (atom nil) (atom nil) (atom nil))
  ;;       node13 (->Node "a" 1 (atom nil) (atom nil) (atom nil) (atom nil))
  ;;       node11 (->Node "a" 1 (atom nil) (atom nil) (atom node12) (atom node13))
  ;;       node21 (->Node "a" 1 (atom nil) (atom nil) (atom nil) (atom nil))
  ;;       node1 (->Node "a" 1 (atom nil) (atom nil) (atom node11) (atom node21))
  ;;       ]
  ;;   (println (rbtree-node-size node1))
  ;;   )

  ;; (->
  ;;  (algo/binary-search ["a" "b" "c" "d" "f" "g" "h" "i"] "e")
  ;;  (println))
  
  )
