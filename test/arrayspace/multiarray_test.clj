(ns arrayspace.multiarray-test
  (:require
   [clojure.test :refer :all]
   [arrayspace.multiarray :refer :all]
   [arrayspace.core :refer [make-multi-array]]))

(deftest contiguous-array-creation
  (testing "Contiguous Array Creation"
    (is (not (nil? (make-multi-array :default :type double :shape [5 5 5]))))))

(deftest contiguous-buffer-array-creation
  (testing "Contiguous Buffer Array Creation"
    (is (not (nil? (make-multi-array :local-byte-buffer :type double :shape [5 5 5]))))))

