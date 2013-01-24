(ns arrayspace.distributions.contiguous-buffer
  (:require
   [arrayspace.protocols
    :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
   [arrayspace.core :refer [make-distribution]]
   [arrayspace.types :refer [resolve-type required-storage-size]])
  (:import (java.nio ByteBuffer CharBuffer ShortBuffer
                     IntBuffer LongBuffer FloatBuffer DoubleBuffer)))

(defmacro def-primitive-buffer-dist
  [buf-type]
  (let [bufdist-name (str buf-type "Distribution")
        bufdist-sym (symbol bufdist-name)
        bufvar (with-meta (symbol "buf") {:tag buf-type})
        startvar (with-meta (symbol "start") {:tag Long/TYPE})
        endvar (with-meta (symbol "end") {:tag Long/TYPE})]
    `(defrecord ~bufdist-sym
         [~bufvar ~startvar ~endvar]
       Distribution
       (descriptor [this#]
         {:type (type this#)
          :storage ~bufvar
          :size (.capacity ~bufvar)
          :start ~startvar
          :end ~endvar})
       LinearIndexedAccess
       (get-1d [this# idx#]
         (.get ~bufvar idx#))
       LinearIndexedMutation
       (set-1d! [this# idx# val#]
         (.put ~bufvar idx# val#)))))

(def-primitive-buffer-dist ByteBuffer)
(def-primitive-buffer-dist CharBuffer)
(def-primitive-buffer-dist ShortBuffer)
(def-primitive-buffer-dist IntBuffer)
(def-primitive-buffer-dist LongBuffer)
(def-primitive-buffer-dist FloatBuffer)
(def-primitive-buffer-dist DoubleBuffer)

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
