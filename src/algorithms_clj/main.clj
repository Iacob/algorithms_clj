(ns algorithms-clj.main
  (:gen-class)
  (:require [algorithms-clj.algo :as algo]))

(defn -main
  "main function"
  [& args]
  ;;(println "Hello, World!")

  (->
   (algo/selection-sort ["c" "a" "e" "b" "f" "z"])
   (println))
  
  )
