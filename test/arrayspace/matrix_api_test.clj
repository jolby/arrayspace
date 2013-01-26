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

(deftest ecount-test
  (testing "ecount, count impl"
    (let [data [[1 2 3][4 5 6]]
          m (matrix api/int-partitioned-buffer-impl data)]
      (println "int-partitioned-buffer")
      (println (format "M: %s" m))
      (println (format "ecount: %s" (ecount m)))
      (println (format "data: %s" data))
      (println (format "vec/seq: %s" (vec (seq m))))
      (println (format "count: %s" (count m)))
      (println (format "nth 3: %s" (nth m 3)))
      (is (== (ecount m) 6)))))
  