(ns arrayspace.multiarray
  (:require 
   [arrayspace.protocols :refer [IndexedAccess]]
   [arrayspace.core
    :refer [make-domain make-domain-map make-distribution make-multi-array]]))

(defrecord LocalMultiArray
    [domain data-map distribution]  
  IndexedAccess
  (mget [this idxs]
    (.get-1d distribution (.transform-coords data-map idxs))))

(defmethod make-multi-array :default
  [array-type-kw & {:keys [shape type]}]
  (let [dom (make-domain :default :shape shape)
        dist (make-distribution array-type-kw :count count :type type)
        data-map (make-domain-map :default :domain dom :distribution dist)]    
    (LocalMultiArray. dom data-map dist)))