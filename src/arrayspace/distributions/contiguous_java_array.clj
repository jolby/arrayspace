(ns arrayspace.distributions.contiguous-java-array
  (:require [arrayspace.protocols :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
            [arrayspace.core :refer [make-distribution]]
            [arrayspace.types :refer [resolve-type resolve-type-size]]
            [arrayspace.distribution :refer [set-data-1d!]]))
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
    (try
      (aset array idx val)
    (catch Throwable t
      (do (println (format "%s, idx: %s, val: %s" (.descriptor this) idx val)) nil)))))

(defmethod make-distribution :default
  [type-kw & {:keys [element-count type]}]
  {:pre [(not (nil? element-count))]}
  (let [arr (make-array (resolve-type type) element-count)]
    (ContiguousJavaArrayDistribution. arr element-count)))

(defmethod make-distribution :local-1d-java-array
  [type-kw & {:keys [element-count type data]}]
  {:pre [(not (nil? element-count))]}
  (let [arr (make-array (resolve-type type) element-count)
        dist (ContiguousJavaArrayDistribution. arr element-count)]
    (when  data (set-data-1d! dist data))
    dist))
