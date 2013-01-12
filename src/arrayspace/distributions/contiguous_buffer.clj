(ns arrayspace.distributions.contigous-buffer
  (:require 
   [arrayspace.protocols :refer [Distribution LinearIndexedAccess]]
   [arrayspace.core :refer [make-distribution]]
   [arrayspace.types :refer [resolve-type required-storage-size]])
  (:import (java.nio ByteBuffer)))

(defrecord ByteBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.get buf idx)))

(defrecord CharBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.getChar buf idx)))

(defrecord ShortBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.getShort buf idx)))

(defrecord IntBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.getInt buf idx)))

(defrecord LongBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.getLong buf idx)))

(defrecord FloatBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.getFloat buf idx)))

(defrecord DoubleBufferDistribution
    [^ByteBuffer buf]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage buf
     :size (.capacity buf)})
  LinearIndexedAccess
  (get-1d [this idx]
    (.getDouble buf idx)))

(defn cast-buffer-type [buf type]
  (case (.getName type)
    "byte" buf
    "char" (.asCharBuffer buf)
    "short" (.asShortBuffer buf)
    "int" (.asIntBuffer buf)
    "long" (.asLongBuffer buf)
    "float" (.asFloatBuffer buf)
    "double" (.asDoubleBuffer buf)))

(defn make-buffer-distribution
  [count type]
  (let [type (resolve-type type)
        buf (ByteBuffer/allocate (required-storage-size type count))]    
    (ByteBufferDistribution. (cast-buffer-type buf type))))

(defmethod make-distribution :local-byte-buffer 
  [type-kw & {:keys [count type]}]
  (make-buffer-distribution count type))
