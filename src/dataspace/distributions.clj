(ns dataspace.distributions
  (:require 
   [dataspace.protocols 
    :refer [Domain DomainMap Distribution IndexedAccess LinearIndexedAccess]]
   [dataspace.core 
    :refer [make-domain make-domain-map make-distribution make-multi-array]])
  (:import (java.nio ByteBuffer)))

(defn valid-coords? [coords shape]
  (and (= (count coords) (count shape))
       (every? 
        (fn [kv] 
          (let [[k v] kv]
            (and (>= k 0) (< k v)))) 
        (map vector coords shape))))

(defn check-valid-coords [coords shape]
  (when-not (valid-coords? coords shape)    
    (throw (Exception. (str "Coordinates " (vec coords)
			    " not valid for shape: " (vec shape))))))

(defn strides-of-shape [shape]
  (reverse (reductions * (reverse (concat (drop 1 shape) [1])))))

(defn element-count-of-shape [shape]
  (reduce * shape))
  
(defn flatten-coords [coords shape]
  "Flatten the coordinates in a multidimensional space to 1d"
  (check-valid-coords coords shape)
  (let [strides (strides-of-shape shape)]
    (reduce + (map * coords strides))))

;; Types/Type size calculations
;; Move this to types ns
;;
;; XXX--doesn't need to be atom or dynamic
(def ^:dynamic  *types*
  (atom {:boolean Boolean/TYPE
         :char Character/TYPE
         :byte Byte/TYPE
         :short Short/TYPE
         :int Integer/TYPE
         :long Long/TYPE
         :float Float/TYPE
         :double Double/TYPE
         boolean Boolean/TYPE
         char Character/TYPE
         byte Byte/TYPE
         short Short/TYPE
         int Integer/TYPE
         long Long/TYPE
         float Float/TYPE
         double Double/TYPE
         Boolean/TYPE Boolean/TYPE
         Character/TYPE Character/TYPE
         Byte/TYPE Byte/TYPE 
         Short/TYPE Short/TYPE  
         Integer/TYPE Integer/TYPE 
         Long/TYPE Long/TYPE 
         Float/TYPE Float/TYPE 
         Double/TYPE Double/TYPE
         }))

(def primitive-types  
  #{Boolean/TYPE  Character/TYPE Byte/TYPE  Short/TYPE  Integer/TYPE  Long/TYPE  Float/TYPE  Double/TYPE boolean char byte short int long float double})

(def type-bytesize
  {Boolean/TYPE 8
   Character/TYPE Character/SIZE
   Byte/TYPE Byte/SIZE 
   Short/TYPE Short/SIZE  
   Integer/TYPE Integer/SIZE 
   Long/TYPE Long/SIZE 
   Float/TYPE Float/SIZE 
   Double/TYPE Double/SIZE})

(defn resolve-type [t]
  ;; (cond 
  ;;  (primitive-types t) t
  ;;  (keyword? t) (@*types* t)
  ;;  :else nil)
  (@*types* t)
  )

(defn resolve-type-size [t]
  (get type-bytesize (resolve-type t) 64))

(defn required-storage-size [type count]
  (*  (/ (resolve-type-size type) Byte/SIZE)  count))

(defn cast-buffer-type [buf type]
  (case (.getName type)
    "byte" buf
    "char" (.asCharBuffer buf)
    "short" (.asShortBuffer buf)
    "int" (.asIntBuffer buf)
    "long" (.asLongBuffer buf)
    "float" (.asFloatBuffer buf)
    "double" (.asDoubleBuffer buf)))
;;
;; Simple basic types used in local computations
;;
(defrecord ContiguousArrayDistribution
    [array size-in-bytes]
  Distribution
  (descriptor [this] 
    {:type (type this)
     :storage array
     :size size-in-bytes})
  LinearIndexedAccess
  (get-1d [this idx]
    (aget array idx)))

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

(defrecord OrdinalDomain
    [shape rank strides]
    Domain
    (shape [this] shape)
    (rank [this] rank))

(defrecord LocalBlockDataMap
    [domain distribution]
  DomainMap
  (transform-coords [this coords] 
    (flatten-coords coords (.shape domain))))

(defrecord LocalMultiArray
    [domain data-map distribution]  
  IndexedAccess
  (mget [this idxs]
    (.get-1d distribution (.transform-coords data-map idxs))))

(defn make-buffer-distribution
  [count type]
  (let [type (resolve-type type)
        buf (ByteBuffer/allocate (required-storage-size type count))]    
    (ByteBufferDistribution. (cast-buffer-type buf type))))

(defmethod make-distribution :default 
  [type-kw & {:keys [count type]}]
  (let [arr (make-array (resolve-type type) (*  (/ (resolve-type-size type) Byte/SIZE)  count))]
    (ContiguousArrayDistribution. arr count)))

(defmethod make-distribution :local-1d-java-array 
  [type-kw & {:keys [count type]}]
  (let [arr (make-array (resolve-type type) (*  (/ (resolve-type-size type) Byte/SIZE)  count))]
    (ContiguousArrayDistribution. arr count)))

(defmethod make-distribution :local-byte-buffer 
  [type-kw & {:keys [count type]}]
  (make-buffer-distribution count type))

(defmethod make-multi-array :default
  [array-type-kw & {:keys [shape type]}]
  (let [rank (count shape)
        count (element-count-of-shape shape)
        strides (strides-of-shape shape)
        dom (OrdinalDomain. shape rank strides)
        dist (make-distribution  array-type-kw :count count :type type)
        data-map (LocalBlockDataMap. dom dist)]
    
    (LocalMultiArray. dom data-map dist)))