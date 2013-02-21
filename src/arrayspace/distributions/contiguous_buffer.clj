(ns arrayspace.distributions.contiguous-buffer
  (:require
   [arrayspace.protocols
    :refer [Distribution LinearIndexedAccess LinearIndexedMutation]]
   [arrayspace.core :refer [make-distribution]]
   [arrayspace.types :refer [resolve-type resolve-type-coercion-fn required-storage-size *types*]]
   [arrayspace.distribution :refer [set-data-flat!]])
  (:import (java.nio ByteBuffer CharBuffer ShortBuffer
                     IntBuffer LongBuffer FloatBuffer DoubleBuffer)))

(declare make-buffer-distribution)

(defmacro def-primitive-buffer-dist
  [buf-type element-type]
  (let [bufdist-name (str buf-type "Distribution")
        bufdist-sym (symbol bufdist-name)
        bufvar (with-meta (symbol "buf") {:tag buf-type})
        startvar (with-meta (symbol "start") {:tag Long/TYPE})
        endvar (with-meta (symbol "end") {:tag Long/TYPE})
        coercion-fn (or (resolve-type-coercion-fn element-type)
                        (do (println (format "element-type: %s resolved-type: %s rtt: %s type: %s" element-type (resolve-type element-type) (type (resolve-type element-type)) (type element-type)))
                            ;;(println (format "types: %s" @*types*))
                        (throw (Exception. (str "Unable to resolve coercion fn for: " element-type)))))]
    `(defrecord ~bufdist-sym
         [~bufvar ~startvar ~endvar]
       Distribution
       (descriptor [this#]
         {:type (type this#)
          :element-type ~element-type
          :storage ~bufvar
          :size (.capacity ~bufvar)
          :start ~startvar
          :end ~endvar})
       (copy [this#]
         (let [newdist# (make-buffer-distribution
                        ~element-type
                        (.capacity ~bufvar)
                        ~startvar ~endvar)]
         (set-data-flat! newdist# (map #(.get ~bufvar (int %1)) (range ~startvar ~endvar)))
         newdist#))
       LinearIndexedAccess
       (get-flat [this# idx#]
         (.get ~bufvar (int idx#)))
       LinearIndexedMutation
       (set-flat! [this# idx# val#]
         (.put ~bufvar (int idx#) (~coercion-fn val#))))))

(def-primitive-buffer-dist ByteBuffer Byte/TYPE)
(def-primitive-buffer-dist CharBuffer Character/TYPE)
(def-primitive-buffer-dist ShortBuffer Short/TYPE)
(def-primitive-buffer-dist IntBuffer Integer/TYPE)
(def-primitive-buffer-dist LongBuffer Long/TYPE)
(def-primitive-buffer-dist FloatBuffer Float/TYPE)
(def-primitive-buffer-dist DoubleBuffer Double/TYPE)

(defn cast-buffer-type [buf element-type]
  (case (.getName element-type)
    "byte" buf
    "char" (.asCharBuffer buf)
    "short" (.asShortBuffer buf)
    "int" (.asIntBuffer buf)
    "long" (.asLongBuffer buf)
    "float" (.asFloatBuffer buf)
    "double" (.asDoubleBuffer buf)))

(defn distribution-for-type [buf element-type start end]
  {:pre [(not-any? nil? '(buf element-type start end))]}
  (case (.getName element-type)
    "byte" (ByteBufferDistribution. buf start end)
    "char" (CharBufferDistribution. (cast-buffer-type buf element-type) start end)
    "short" (ShortBufferDistribution. (cast-buffer-type buf element-type) start end)
    "int" (IntBufferDistribution. (cast-buffer-type buf element-type) start end)
    "long" (LongBufferDistribution. (cast-buffer-type buf element-type) start end)
    "float" (FloatBufferDistribution. (cast-buffer-type buf element-type) start end)
    "double" (DoubleBufferDistribution. (cast-buffer-type buf element-type) start end)
    (throw (Exception. (str "Don't know how to build distribution for element-type: "
                            element-type "named: "(.getName element-type))))))

(defn make-buffer-distribution
  [element-type element-count start end]
  (let [element-type (resolve-type element-type)
        buf (ByteBuffer/allocate (required-storage-size element-type element-count))]
    (distribution-for-type buf element-type start end)))

(defmethod make-distribution :local-byte-buffer
  [type-kw & {:keys [element-count element-type start end data]
              :or {start 0}}]
  {:pre [(not (nil? element-count))]}
  (let [dist (make-buffer-distribution element-type element-count start
                                       (or end (dec element-count)))]
    (when  data (set-data-flat! dist data))
    dist))
