(ns arrayspace.distributions.contiguous-java-array
  (:require [arrayspace.protocols :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
            [arrayspace.core :refer [make-distribution]]
            [arrayspace.types :refer [resolve-type resolve-type-coercion-fn resolve-type-size]]
            [arrayspace.distribution :refer [set-data-flat!]]))
;;
;; Simple basic type used in local computations
;;
(defrecord ContiguousJavaArrayDistribution
    [array element-type coercion-fn size-in-bytes]
  Distribution
  (descriptor [this]
    {:type (type this)
     :element-type element-type
     :storage array
     :size size-in-bytes})
  (copy [this]
    (let [newarr (into-array element-type array)]
      (ContiguousJavaArrayDistribution. newarr element-type coercion-fn size-in-bytes)))
  LinearIndexedAccess
  (get-flat [this idx]
    (aget array idx))
  LinearIndexedMutation
  (set-flat! [this idx val]
    (try
      (aset array idx (coercion-fn val))
    (catch Throwable t
      (do (println (format "%s, idx: %s, val: %s" (.descriptor this) idx val)) nil)))))

(defmethod make-distribution :default
  [type-kw & {:keys [element-count element-type]}]
  {:pre [(not (nil? element-count))]}
  (let [arr (make-array (resolve-type element-type) element-count)
        coercion-fn (resolve-type-coercion-fn (resolve-type element-type))]
    (ContiguousJavaArrayDistribution. arr element-type coercion-fn element-count)))

(defmethod make-distribution :local-1d-java-array
  [type-kw & {:keys [element-count element-type data]}]
  {:pre [(not (nil? element-count))]}
  (let [arr (make-array (resolve-type element-type) element-count)
        coercion-fn (resolve-type-coercion-fn (resolve-type element-type))
        dist (ContiguousJavaArrayDistribution. arr element-type coercion-fn element-count)]
    (when  data (set-data-flat! dist data))
    dist))
