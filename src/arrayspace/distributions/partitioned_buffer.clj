(ns arrayspace.distributions.partitioned_buffer
    (:require
     [arrayspace.protocols
      :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
     [arrayspace.core :refer [make-distribution]]
     [arrayspace.types
      :refer [resolve-type required-storage-size]]
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
  

(defrecord IntBufferDistributionChunk
    [^IntBuffer buf ^long start ^long end]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)
     :start start
     :end end })
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defrecord PartitionedIntBufferDistribution
    [buffer-chunks ^long size]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buffer-chunks
     :size size})
  LinearIndexedAccess
  (get-1d [this idx]
    (when-let [chunk (binary-range-search buffer-chunks idx chunk-contains-index? chunk-lt-index?)]
      (.get (.buf chunk) (- idx (.start chunk)))))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (when-let [chunk (binary-range-search buffer-chunks idx chunk-contains-index? chunk-lt-index?)]
    (.put (.buf chunk) (- idx (.start chunk)) val))))

(defn make-partition-chunks
  [partition-count count-per-buf chunk-storage-size]
  (loop [accum []
         pnum 0
         pstart 0
         pend (dec count-per-buf)]
    (if (= pnum partition-count)
      accum
      (recur (conj accum (IntBufferDistributionChunk. (.asIntBuffer (ByteBuffer/allocate chunk-storage-size)) pstart pend))
             (inc pnum)
             (+ pstart count-per-buf)
             (+ pend count-per-buf)))))
         
(defn make-partitioned-buffer-distribution
  [count partition-count type]
  (let [type (resolve-type type)
        count-per-buf (/ count partition-count)
        chunk-storage-size (required-storage-size type count-per-buf)
        buffer-chunks (make-partition-chunks partition-count count-per-buf chunk-storage-size)]
    (PartitionedIntBufferDistribution. buffer-chunks count)))

(defmethod make-distribution :partitioned-byte-buffer 
  [type-kw & {:keys [count partition-count type]}]
  (make-partitioned-buffer-distribution count partition-count type))
