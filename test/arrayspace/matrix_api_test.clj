(ns arrayspace.matrix-api-test
    (:require
     [clojure.test :refer :all]
     [arrayspace.multiarray :refer :all]     
     [arrayspace.matrix-api :as api]
     [core.matrix :refer :all]
     [core.matrix.compliance-tester]))

 ;; (deftest java-array-compliance-test
 ;;   (testing "Local Contiguous Java Array Distributions"
 ;;     (core.matrix.compliance-tester/compliance-test api/int-local-1d-java-array-impl)))

;; (deftest local-buffer-compliance-test
;;   (testing "Local Contiguous Buffer Distributions"
;;   (core.matrix.compliance-tester/compliance-test api/int-local-buffer-impl)))

;; (deftest partitioned-buffer-compliance-test
;;   (testing "Partitioned Contiguous Buffer Distributions"
;;   (core.matrix.compliance-tester/compliance-test api/int-partitioned-buffer-impl)))

(deftest basic-api-test
  (testing "Basic API implementation. Sanity checks"
    (let [data2d [[1 2 3][4 5 6][7 8 9]]
          data3d [[[1 2 3][4 5 6][7 8 9]]
                  [[10 11 12][13 14 15][16 17 18]]
                  [[19 20 21][22 23 24][25 26 27]]]
          m2 (matrix api/int-local-buffer-impl data2d)
          m3 (matrix api/int-local-buffer-impl data3d)]
      (println "int-partitioned-buffer")
      (println (format "M2: %s" m2))
      (println (format "ecount: %s" (ecount m2)))
      (println (format "data2d: %s" data2d))
      (println (format "data3d: %s" data3d))
      (println (format "vec/seq: %s" (vec (seq m2))))
      (println (format "count: %s" (count m2)))
      (println (format "nth 3: %s" (nth m2 3)))
      (println (format "row 1: %s" (vec (get-row m2 1))))
      (println (format "col 1: %s" (vec (get-column m2 1))))
      (println (format "slice 1: %s" (.get-slice m2 1 1)))
      (println (format "slice 1: %s" (.get-slice m3 2 2)))
      (println m3)
      (api/elwise-fn m3 #(println (format "Got element: %d" %1)))
      (is (== (ecount m2) 9)))))
  