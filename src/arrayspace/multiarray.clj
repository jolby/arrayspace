(ns arrayspace.multiarray
  (:require
   [arrayspace.protocols :refer [IndexedAccess IndexedMutation]]
   [arrayspace.core
    :refer [make-domain make-domain-map make-distribution make-multi-array]]
   [arrayspace.domain :refer [element-count-of-shape]]
   [arrayspace.distributions.contiguous-java-array]
   [arrayspace.distributions.contiguous-buffer]))

(defrecord LocalMultiArray
    [domain data-map distribution]
  IndexedAccess
  (mget [this idxs]
    (.get-1d distribution (.transform-coords data-map idxs)))
  IndexedMutation
  (mset! [this idxs val]
    (.set-1d! distribution (.transform-coords data-map idxs) val)))

(defmethod make-multi-array :default
  [array-type-kw & {:keys [domain distribution data-map shape type]}]
  (let [dom (or domain (make-domain :default :shape shape))
        dist (or distribution (make-distribution array-type-kw
                                                 :element-count (element-count-of-shape shape) :type type))
        data-map (or data-map (make-domain-map :default :domain dom :distribution dist))]
    (LocalMultiArray. dom data-map dist)))