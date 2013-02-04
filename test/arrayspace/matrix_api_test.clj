(ns arrayspace.matrix-api-test
    (:require
     [clojure.test :refer :all]
     [arrayspace.multiarray :refer :all]
     [arrayspace.matrix-api :as api]
     [core.matrix :refer :all]
     [core.matrix.protocols :refer [get-slice]]
     [core.matrix.compliance-tester]))

(deftest java-array-compliance-test
  (testing "Local Contiguous Java Array Distributions"
    (core.matrix.compliance-tester/compliance-test api/int-local-1d-java-array-impl)))

;; (deftest local-buffer-compliance-test
;;   (testing "Local Contiguous Buffer Distributions"
;;     (core.matrix.compliance-tester/compliance-test api/int-local-buffer-impl)))

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
      ;;(println "int-partitioned-buffer")
      ;;(println (format "M2: %s" m2))
      (println (format "ecount: %s" (ecount m2)))
      ;;(println (format "data2d: %s" data2d))
      ;;(println (format "data3d: %s" data3d))
      (println (format "vec/seq: %s" (vec (seq m2))))
      (println (format "count: %s" (count m2)))
      (println (format "nth 3: %s" (nth m2 3)))
      ;;(println m3)
      (api/elwise-fn m3 #(assert (not (nil? %2))))
      (is (== (ecount m2) 9)))))


(deftest slice-test
  (letfn [(print-slice [fmt-str m]
            (println (format fmt-str (if (array? m) [(vec (shape m)) (vec (seq  m))] m))))]
  (testing "Basic slicing operations"
    (let [data2d [[1 2 3][4 5 6][7 8 9]]

          data3d [[[ 1  2  3]
                   [ 4  5  6]
                   [ 7  8  9]]
                  [[10 11 12]
                   [13 14 15]
                   [16 17 18]]
                  [[19 20 21]
                   [22 23 24]
                   [25 26 27]]]

          m2 (matrix api/int-local-buffer-impl data2d)
          m3 (matrix api/int-local-buffer-impl data3d)
          m4 (api/make-arrayspace-matrix :int-local-buffer
                                     :local-byte-buffer
                                     :shape [4 4 4 4]
                                     :type int)]
      ;; (println (format "m3 (slice m3 1 1) %s" (get-slice m3 1 1)))
      ;; (println (format "m4 (slice m4 1 1) %s" (get-slice m4 1 1)))
      ;; (println (format "m4 (slice m4 0 1) %s" (get-slice m4 0 1)))
      ;; (println (format "m4 (slice m4 2 2) %s" (get-slice m4 2 2)))
      (println (format "row 1: %s" (vec (seq (get-row m2 1)))))
      (println (format "col 1: %s" (vec (seq (get-column m2 1)))))
      (println (format "slice m2 1 1: %s" (get-slice m2 1 1)))
      (println (format "slice m3 2 2: %s" (get-slice m3 2 2)))

      ;;dim0 0 - 2
      (print-slice "m3 (slice m3 0 0) %s" (get-slice m3 0 0))
      (print-slice "m3 (slice m3 0 1) %s" (get-slice m3 0 1))
      (print-slice "m3 (slice m3 0 2) %s" (get-slice m3 0 2))
      ;;dim1 0 - 8
      (print-slice "m3 (slice m3 1 0) %s" (get-slice m3 1 0))
      (print-slice "m3 (slice m3 1 1) %s" (get-slice m3 1 1))
      (print-slice "m3 (slice m3 1 8) %s" (get-slice m3 1 8))
      ;;dim2 0 - 26
      (print-slice "m3 (slice m3 2 0) %s" (get-slice m3 2 0))
      (print-slice "m3 (slice m3 2 1) %s" (get-slice m3 2 1))
      (print-slice "m3 (slice m3 2 2) %s" (get-slice m3 2 2))
      (print-slice "m3 (slice m3 2 15) %s" (get-slice m3 2 15))
      (print-slice "m3 (slice m3 2 26) %s" (get-slice m3 2 26))
      ;;(print-slice "m4 (slice m4 1 1) %s"  (get-slice m4 1 1))
      ))))