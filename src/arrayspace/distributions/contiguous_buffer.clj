(ns arrayspace.distributions.contigous-buffer
  (:require 
   [arrayspace.protocols :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
   [arrayspace.core :refer [make-distribution]]
   [arrayspace.types :refer [resolve-type required-storage-size]])
  (:import (java.nio ByteBuffer CharBuffer ShortBuffer 
                     IntBuffer LongBuffer FloatBuffer DoubleBuffer)))

(defrecord ByteBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defrecord CharBufferDistribution
    [^CharBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defrecord ShortBufferDistribution
    [^ShortBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defrecord IntBufferDistribution
    [^IntBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defrecord LongBufferDistribution
    [^LongBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defrecord FloatBufferDistribution
    [^FloatBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defrecord DoubleBufferDistribution
    [^DoubleBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx))
  LinearIndexedMutation
  (set-1d! [this idx val]
    (.put buf idx val)))

(defn cast-buffer-type [buf type]
  (case (.getName type)
    "byte" buf
    "char" (.asCharBuffer buf)
    "short" (.asShortBuffer buf)
    "int" (.asIntBuffer buf)
    "long" (.asLongBuffer buf)
    "float" (.asFloatBuffer buf)
    "double" (.asDoubleBuffer buf)))

(defn distribution-for-type [buf type]
  (case (.getName type)
    "byte" (ByteBufferDistribution. buf)
    "char" (CharBufferDistribution. (cast-buffer-type buf type))
    "short" (ShortBufferDistribution. (cast-buffer-type buf type))
    "int" (IntBufferDistribution. (cast-buffer-type buf type))
    "long" (LongBufferDistribution. (cast-buffer-type buf type))
    "float" (FloatBufferDistribution. (cast-buffer-type buf type))
    "double" (DoubleBufferDistribution. (cast-buffer-type buf type))))


(defn make-buffer-distribution
  [count type]
  (let [type (resolve-type type)
        buf (ByteBuffer/allocate (required-storage-size type count))]    
    (distribution-for-type buf type)))

(defmethod make-distribution :local-byte-buffer 
  [type-kw & {:keys [count type]}]
  (make-buffer-distribution count type))
