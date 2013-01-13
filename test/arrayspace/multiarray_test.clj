(ns arrayspace.multiarray-test
  (:require
   [clojure.test :refer :all]
   [arrayspace.multiarray :refer :all]
   [arrayspace.core :refer [mget mset! make-multi-array]]))

(deftest contiguous-array-creation
  (testing "Contiguous Array Creation"
    (is (not (nil? (make-multi-array :default :type double :shape [5 5 5]))))))

(deftest contiguous-buffer-array-creation
  (testing "Contiguous Buffer Array Creation"
    (is (not (nil? (make-multi-array :local-byte-buffer :type double :shape [5 5 5]))))))

(deftest mget-test
  (testing "mget implementation"
    (let [a1 (make-multi-array :default :type double :shape [5])
          buf1 (make-multi-array :local-byte-buffer :type double :shape [5])
          a2 (make-multi-array :default :type double :shape [5 5])
          buf2 (make-multi-array :local-byte-buffer :type double :shape [5 5])
          a3 (make-multi-array :default :type double :shape [5 5 5])
          buf3 (make-multi-array :local-byte-buffer :type double :shape [5 5 5])
          a4 (make-multi-array :default :type double :shape [5 5 5 5])
          buf4 (make-multi-array :local-byte-buffer :type double :shape [5 5 5 5])]   
      (are [x y] (= x y)
           0.0 (mget a1 1)
           0.0 (mget buf1 1)
           0.0 (mget a2 0 1)
           0.0 (mget buf2 0 1)
           0.0 (mget a3 0 0 1)
           0.0 (mget buf3 0 0 1)
           0.0 (mget a4 0 0 0 1)
           0.0 (mget buf4 0 0 0 1)))))

(deftest mset!-test
  (testing "mset! implementation"
    (let [a1 (make-multi-array :default :type double :shape [5])
          buf1 (make-multi-array :local-byte-buffer :type double :shape [5])
          a2 (make-multi-array :default :type double :shape [5 5])
          buf2 (make-multi-array :local-byte-buffer :type double :shape [5 5])
          a3 (make-multi-array :default :type double :shape [5 5 5])
          buf3 (make-multi-array :local-byte-buffer :type double :shape [5 5 5])
          a4 (make-multi-array :default :type double :shape [5 5 5 5])
          buf4 (make-multi-array :local-byte-buffer :type double :shape [5 5 5 5])]
      (mset! a1 1 2.2)
      (mset! buf1 1 2.2)
      (mset! a2 0 1 2.2)
      (mset! buf2 0 1 2.2)
      (mset! a3 0 0 1 2.2)
      (mset! buf3 0 0 1 2.2)
      (mset! a4 0 0 0 1 2.2)
      (mset! buf4 0 0 0 1 2.2)
      (are [x y] (= x y)
           2.2 (mget a1 1)
           2.2 (mget buf1 1)
           2.2 (mget a2 0 1)
           2.2 (mget buf2 0 1)
           2.2 (mget a3 0 0 1)
           2.2 (mget buf3 0 0 1)
           2.2 (mget a4 0 0 0 1)
           2.2 (mget buf4 0 0 0 1)))))