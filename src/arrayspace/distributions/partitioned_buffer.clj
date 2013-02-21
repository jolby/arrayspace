(ns arrayspace.distributions.partitioned_buffer
    (:require
     [arrayspace.protocols
      :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
     [arrayspace.core :refer [make-distribution]]
     [arrayspace.types
      :refer [resolve-type required-storage-size]]
     [arrayspace.distribution :refer [set-data-flat!]]
     [arrayspace.distributions.contiguous-buffer
      :refer [cast-buffer-type distribution-for-type make-buffer-distribution]]))

(defn binary-range-search
  "Find element whose low/hi indicies contain idx"
  [col ^long idx contains-fn lt-fn]
  (loop [low 0
         hi (unchecked-dec (count col))
         low-el (nth col low)
         hi-el (nth col hi)]
    (if (<= hi (inc low))
         (cond
           (contains-fn low-el idx) low-el
           (contains-fn hi-el idx) hi-el
           :else nil)
         (let [mid (unchecked-add low (bit-shift-right (unchecked-subtract hi low) 1))
               mid-el (nth col mid)]
           (if (lt-fn mid-el idx)
             (recur (unchecked-inc mid) hi (nth col (unchecked-inc mid)) (nth col hi))
             (recur low mid (nth col low) (nth col hi)))))))

(defn chunk-contains-index?
  "Return true if index is between buffer's start/end range (inclusive)"
  [bufchunk ^long idx]
  (and (<= (.start bufchunk) idx)
       (<= idx (.end bufchunk))))

(defn chunk-lt-index?
  "Return true if this chunk end idx is less than idx"
  [bufchunk ^long idx]
  (< (.end bufchunk) idx))

(defrecord PartitionedIntBufferDistribution
    [buffer-chunks ^long size]
  Distribution
  (descriptor [this]
    {:type (type this)
     :storage buffer-chunks
     :size size})
  (copy [this] (throw (UnsupportedOperationException.)))
  LinearIndexedAccess
  (get-flat [this idx]
    (when-let [chunk (binary-range-search buffer-chunks idx chunk-contains-index? chunk-lt-index?)]
      ;;(println this)
      ;;(println (format "Got idx: %d, real addr: %d" idx (- idx (.start chunk))))
      (.get (.buf chunk) (int (- idx (.start chunk))) )))
  LinearIndexedMutation
  (set-flat! [this idx val]
    (when-let [chunk (binary-range-search buffer-chunks idx chunk-contains-index? chunk-lt-index?)]
    (.put (.buf chunk) (- idx (.start chunk)) val))))

(defn make-partition-chunks
  [element-type partition-count count-per-buf chunk-storage-size]
  (loop [accum []
         pnum 0
         pstart 0
         pend (dec count-per-buf)]
    (if (= pnum partition-count)
      accum
      (recur (conj accum (make-buffer-distribution element-type chunk-storage-size pstart pend))
             (inc pnum)
             (+ pstart count-per-buf)
             (+ pend count-per-buf)))))

(defn make-partitioned-buffer-distribution
  [element-type element-count partition-count]
  (let [type (resolve-type element-type)
        count-per-buf (/ element-count partition-count)
        chunk-storage-size (required-storage-size type count-per-buf)
        buffer-chunks (make-partition-chunks type partition-count count-per-buf chunk-storage-size)]
    (PartitionedIntBufferDistribution. buffer-chunks element-count)))

(defmethod make-distribution :partitioned-byte-buffer
  [type-kw & {:keys [element-count partition-count element-type data]}]
  {:pre [(not (nil? element-count))
         (not (nil? partition-count))]}
  (let [dist (make-partitioned-buffer-distribution element-type element-count partition-count)]
    (when  data (set-data-flat! dist data))
    dist))
