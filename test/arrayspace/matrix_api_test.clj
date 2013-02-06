(ns arrayspace.matrix-api-test
    (:require
     [clojure.test :refer :all]
     [arrayspace.multiarray :refer :all]
     [arrayspace.matrix-api :as api]
     [core.matrix :refer :all]
     [core.matrix.protocols :refer [get-slice element-seq]]
     [core.matrix.compliance-tester]
     [core.matrix.impl.persistent-vector]))

(def ^:dynamic data2d nil)
(def ^:dynamic data3d nil)
(def ^:dynamic m2 nil)
(def ^:dynamic m3 nil)

(defn basic-array-fixtures [f]
  (let [d2d [[1 2 3][4 5 6][7 8 9]]
        d3d [[[1 2 3][4 5 6][7 8 9]]
             [[10 11 12][13 14 15][16 17 18]]
             [[19 20 21][22 23 24][25 26 27]]]
        ma2 (matrix api/int-local-buffer-impl d2d)
        ma3 (matrix api/int-local-buffer-impl d3d)]
    (binding [data2d d2d
              data3d d3d
              m2 ma2
              m3 ma3]
      (f))))

(use-fixtures :each basic-array-fixtures)


;; (deftest java-array-compliance-test
;;  (testing "Local Contiguous Java Array Distributions"
;;    (core.matrix.compliance-tester/compliance-test api/double-local-1d-java-array-impl)))

;; (deftest local-buffer-compliance-test
;;   (testing "Local Contiguous Buffer Distributions"
;;     (core.matrix.compliance-tester/compliance-test api/int-local-buffer-impl)))

;; (deftest partitioned-buffer-compliance-test
;;   (testing "Partitioned Contiguous Buffer Distributions"
;;   (core.matrix.compliance-tester/compliance-test api/int-partitioned-buffer-impl)))

(deftest basic-api-test
  (testing "do-elements works"
    (api/do-elements m3 #(assert (not (nil? %1)))))
  
  (testing "ecount and count"
    (is (== (ecount m2) 9))
    (is (== (count m2) 3))
    (is (== (ecount m3) 27))
    (is (== (count m3) 3)))

  (testing "equality/equivalence"
    (is (= m2 m2))
    (is (equals m2 data2d))
    (is (= m3 m3))
    (is (equals m3 data3d)))

  (testing "basic type introspection and predicates"
    (is (true? (array? m2)))
    (is (true? (matrix? m2)))
    (is (true? (array? m3))))
    
    ;; (println (format "scale m2 X 2: %s" (vec (scale m2 2))))
    ;; (println (format "scale m3 X 2: %s" (vec (scale m3 2))))
    ;; (println (format "m3 assign!: %s" (vec (assign! m3 (vec (range 27 (* 2 27)))))))
    )

(deftest slice-test
  (letfn [(do-slice [m dim idx exp-shp exp-dat]
            (let [res (slice m dim idx)
                  [shp dat] (if (array? res)
                              [(shape res) res]
                              [[1] [res]])]
              (is (= shp exp-shp))
              (is (equals dat exp-dat))))]
    (do-slice m3 0 0 [3 3] [[1 2 3][4 5 6][7 8 9]])
    (do-slice m3 0 1 [3 3] [[10 11 12][13 14 15][16 17 18]])
    (do-slice m3 1 0 [3] [1 2 3])
    (do-slice m3 1 8 [3] [25 26 27])
    (do-slice m3 2 0 [1] [1])
    (do-slice m3 2 26 [1] [27])))