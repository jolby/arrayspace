(ns arrayspace.distributions.contiguous-java-array
  (:require [arrayspace.protocols :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
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
    (aget array idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (aset array idx val)))

(defmethod make-distribution :default 
  [type-kw & {:keys [count type]}]
  (let [arr (make-array (resolve-type type) count)]
    (ContiguousJavaArrayDistribution. arr count)))

(defmethod make-distribution :local-1d-java-array 
  [type-kw & {:keys [count type]}]
  (let [arr (make-array (resolve-type type) count)]
    (ContiguousJavaArrayDistribution. arr count)))

