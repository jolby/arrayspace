(ns arrayspace.multiarray-test
  (:require
   [clojure.test :refer :all]
   [arrayspace.multiarray :refer :all]
   [arrayspace.core :refer [mget mset! make-distribution make-multi-array]]
   [arrayspace.domain :refer [element-count-of-shape]]
   [arrayspace.distributions.partitioned-buffer]))

;;
;; Utility functions
;;

(defn n-dim-progression
  "Create a progression of dimensions from 1 to dim
with each dim having count-per-dim elements"
  [dim count-per-dim]
  (reductions conj [count-per-dim] (take (dec dim) (repeat count-per-dim))))

(defn n-dim-indx
  "Create n-dim index with last dim of value idx"
  [dim idx]
  (map vec (reductions conj (list idx) (take (dec dim) (repeat 0)))))

(defn jarr [element-type shape]
  (make-multi-array :default :element-type element-type :shape shape))

(defn buffarr [element-type shape]
  (make-multi-array :local-byte-buffer :element-type element-type :shape shape))

(defn pbuffarr [element-type shape]
  (let [element-count (element-count-of-shape shape)
        pdist (make-distribution :partitioned-byte-buffer
                                 :element-type element-type
                                 :element-count element-count
                                 :partition-count (count shape))]
    (make-multi-array :partitioned-byte-buffer
                      :element-type element-type
                      :shape shape
                      :element-count element-count
                      :distribution pdist)))

(defn test-mget-for-type [element-type type-val shape shape-idx]
  (let [a (jarr element-type shape)
        buf (buffarr element-type shape)
        pbuf (pbuffarr element-type shape)]
    (mset! a shape-idx type-val)
    (mset! buf shape-idx type-val)
    (mset! pbuf shape-idx type-val)
    (are [x y] (= x y)
         type-val (mget a shape-idx)
         type-val (mget buf shape-idx)
         type-val (mget pbuf shape-idx))))

;;
;; Fixtures
;;
(def ^:dynamic *primitive-types* [byte char short int long float double])

(def ^:dynamic *type-vals* [(byte 2) (char \a) (short 2)
                  (int 2) (long 2) (float 2.2) (double 2.2)])

(def ^:dynamic *shapes* (n-dim-progression 5 5))

(def ^:dynamic *indexes* (n-dim-indx 5 1))

(deftest contiguous-array-creation
  (testing "Contiguous Array Creation"
    (is (not (nil? (jarr double [5 5 5]))))))

(deftest contiguous-buffer-array-creation
  (testing "Contiguous Buffer Array Creation"
    (is (not (nil? (buffarr double [5 5 5]))))))

(deftest partitioned-buffer-array-creation
  (testing "Partitioned Buffer Array Creation"
    (is (not (nil? (pbuffarr double [5 5 5]))))))

(deftest mget-test
  (testing "mget implementation"
    (let [a1 (jarr double [5])
          buf1 (buffarr double [5])
          a2 (jarr double [5 5])
          buf2 (buffarr double [5 5])
          a3 (jarr double [5 5 5])
          buf3 (buffarr double [5 5 5])
          a4 (jarr double [5 5 5 5])
          buf4 (buffarr double [5 5 5 5])]
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
    (let [a1 (jarr double [5])
          buf1 (buffarr double [5])
          a2 (jarr double [5 5])
          buf2 (buffarr double [5 5])
          a3 (jarr double [5 5 5])
          buf3 (buffarr double [5 5 5])
          a4 (jarr double [5 5 5 5])
          buf4 (buffarr double [5 5 5 5])]
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

(deftest mget-types-test
  "A more thourough version of above two tests"
  (testing "mget over all primitive types"
    (dorun (map (fn [type type-val]
                  (dorun (map (fn [shape shape-idx]
                                (test-mget-for-type type type-val
                                                    shape shape-idx))
                              *shapes* *indexes*)))
                *primitive-types* *type-vals*))))
