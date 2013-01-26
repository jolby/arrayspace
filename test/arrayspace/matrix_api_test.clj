(ns arrayspace.matrix-api-test
    (:require
     [clojure.test :refer :all]
     [arrayspace.multiarray :refer :all]
     
     [arrayspace.matrix-api :as api]
     [core.matrix.compliance-tester]))

(deftest compliance-test
  (core.matrix.compliance-tester/compliance-test api/int-local-1d-java-array-impl))