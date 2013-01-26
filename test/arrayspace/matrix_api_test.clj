(ns arrayspace.matrix-api-test
    (:require
     [clojure.test :refer :all]
     [arrayspace.multiarray :refer :all]
     
     [arrayspace.matrix-api :as api]
     [core.matrix.compliance-tester]))

(deftest java-array-compliance-test
  (testing "Local Contiguous Java Array Distributions"
    (core.matrix.compliance-tester/compliance-test api/int-local-1d-java-array-impl)))

(deftest local-buffer-compliance-test
  (testing "Local Contiguous Buffer Distributions"
  (core.matrix.compliance-tester/compliance-test api/int-local-buffer-impl)))

