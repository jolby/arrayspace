(ns arrayspace.distributions.contiguous-buffer
  (:require
   [arrayspace.protocols
    :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
   [arrayspace.core :refer [make-distribution]]
   [arrayspace.types :refer [resolve-type required-storage-size]]
   [arrayspace.distribution :refer [set-data-1d!]])
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
         ;;(println (format "Got idx: %d" idx#))
         (.get ~bufvar (int idx#)))
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

(defn distribution-for-type [buf type start end]
  (case (.getName type)
    "byte" (ByteBufferDistribution. buf start end)
    "char" (CharBufferDistribution. (cast-buffer-type buf type) start end)
    "short" (ShortBufferDistribution. (cast-buffer-type buf type) start end)
    "int" (IntBufferDistribution. (cast-buffer-type buf type) start end)
    "long" (LongBufferDistribution. (cast-buffer-type buf type) start end)
    "float" (FloatBufferDistribution. (cast-buffer-type buf type) start end)
    "double" (DoubleBufferDistribution. (cast-buffer-type buf type) start end)))

(defn make-buffer-distribution
  [type element-count start end]
  (let [type (resolve-type type)
        buf (ByteBuffer/allocate (required-storage-size type element-count))]
    (distribution-for-type buf type start end)))

(defmethod make-distribution :local-byte-buffer
  [type-kw & {:keys [element-count type start end data]
              :or {start 0}}]
  {:pre [(not (nil? element-count))]}
  (let [dist (make-buffer-distribution type element-count start
                                       (or end (dec element-count)))]
    (when  data (set-data-1d! dist data))
    dist))
