(ns arrayspace.distributions.contiguous-java-array
  (:require [arrayspace.protocols :refer [Distribution LinearIndexedAccess]]
            [arrayspace.core :refer [make-distribution]]
            [arrayspace.types :refer [resolve-type resolve-type-size]]))
;;
;; Simple basic type used in local computations
;;
(defrecord ContiguousJavaArrayDistribution
    [array size-in-bytes]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage array
     :size size-in-bytes})
  LinearIndexedAccess
  (get-1d [this idx]
    (aget array idx)))

(defmethod make-distribution :default 
  [type-kw & {:keys [count type]}]
  (let [arr (make-array (resolve-type type) (*  (/ (resolve-type-size type) Byte/SIZE)  count))]
    (ContiguousJavaArrayDistribution. arr count)))

(defmethod make-distribution :local-1d-java-array 
  [type-kw & {:keys [count type]}]
  (let [arr (make-array (resolve-type type) (*  (/ (resolve-type-size type) Byte/SIZE)  count))]
    (ContiguousJavaArrayDistribution. arr count)))

