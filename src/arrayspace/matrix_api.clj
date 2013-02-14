(ns arrayspace.matrix-api
  (:require
   [clojure.tools.macro :as macro]
   [arrayspace.protocols :refer :all :exclude [get-1d]]
   [arrayspace.core :refer [make-domain make-domain-map make-distribution]]
   [arrayspace.domain :refer [strides-of-shape element-count-of-shape
                              flatten-coords do-elements-loop]]
   [arrayspace.distributions.contiguous-java-array]
   [arrayspace.distributions.contiguous-buffer]
   [arrayspace.distributions.partitioned-buffer]
   [arrayspace.types :refer [resolve-type resolve-type-from-data]]
   [core.matrix.protocols :refer :all]
   [core.matrix :refer [scalar? array?]]
   [core.matrix.implementations :as imp]
   [core.matrix.impl.persistent-vector]))

(declare make-arrayspace-matrix do-elements do-elements-indexed do-elements!)


(defn TODO []
  ;;(throw (Exception. "TODO- NOT IMPLEMENTED YET"))
  nil)

(deftype ArrayspaceMatrix
    [api domain domain-map distribution element-type]

  Object
  (equals [m o]
    (if (and (array? o)
             (every? true? (map == (get-shape m) (get-shape o)))
             (every? true? (map == (element-seq m) (element-seq o))))
      true
      (do
        (let [oa (array? o)
              shape-eq (every? true? (map == (get-shape m) (get-shape o)))
              el-eq (every? true? (map == (element-seq m) (element-seq o)))]
          (println (format "EQ: FALSE: %s %s" m o))
          (println (format "o: array? %s, shape-eq: %s el-eq: %s" oa shape-eq el-eq))
          (println (format "m-shape: %s, o-shape: %s" (vec (get-shape m)) (vec (get-shape o))))
          (println (format "m-seq: %s, o-seq: %s" (vec (element-seq m)) (vec (element-seq o)))))
        false)))

  (hashCode [m]
    (let [coll (element-seq m)]
      (reduce #(hash-combine %1 (.hashCode %2)) (.hashCode (first coll)) (next coll))))

  clojure.lang.Counted
  (count [m]
    "Return count of first dim. Use ecount for count of all elements"
    (first (get-shape m)))

  clojure.lang.Indexed
  (nth [m i]
    (get-slice m 0 i))

  clojure.lang.Sequential

  clojure.lang.Seqable
  (seq [m]
    (map (fn [idx] (get-slice m 0 idx)) (range (first (shape m)))))

  Domain
  (shape [m] (vec (shape domain)))
  (rank [m] (rank domain))

  PImplementation
  (implementation-key [m] (implementation-key api))
  (construct-matrix [m data] (construct-matrix api data))
  (new-vector [m length] (new-vector api length))
  (new-matrix [m rows columns] (new-matrix api rows columns))
  (new-matrix-nd [m shape] (new-matrix-nd api shape))
  (supports-dimensionality? [m dimensions]
    (supports-dimensionality? api dimensions))

  PDimensionInfo
  (dimensionality [m] (rank domain))
  (get-shape [m] (vec (shape domain)))
  (is-scalar? [m] false)
  (is-vector? [m] (= 1 (rank domain)))
  (dimension-count [m dimension-number]
    (nth (shape domain) dimension-number))

  PTypeInfo
  (element-type [m] element-type)

  PMatrixEquality
  (matrix-equals [m o] (.equals m o))

  PIndexedAccess
  (get-1d [m row]
          (assert (= (rank domain) 1))
          (.get-1d distribution row))
  (get-2d [m row column]
          (assert (= (rank domain) 2))
          (.get-1d distribution (transform-coords domain-map [row column])))
  (get-nd [m indexes]
          (.get-1d distribution (transform-coords domain-map indexes)))

  PIndexedSetting
  (set-1d [m row v]
          (assert (= (rank domain) 1))
          (.set-1d! distribution row v))
  (set-2d [m row column v]
          (assert (= (rank domain) 2))
          (.set-1d! distribution (transform-coords domain-map [row column]) v))
  (set-nd [m indexes v]
          (.set-1d! distribution (transform-coords domain-map indexes) v))
  (is-mutable? [m] true)

  PMatrixSlices
  (get-row [m i]
    (assert (= (rank domain) 2))
    (get-slice m 0 i))

  (get-column [m i]
    ;;XXX--TODO create col-major domain/domain map over this distribution
    (assert (= (rank domain) 2))
    (map #(.get-1d distribution
                   (transform-coords domain-map [%1 i]))
         (range (dimension-count m 1))))

  (get-major-slice [m i]
    (get-slice m 0 i))

  (get-slice [m dimension i]
    {:pre [(and (>= dimension 0) (>= (dec (rank m)) dimension))]}
    (let [shape (long-array (shape m))
          strides (.strides (.domain-map m))
          new-shape (drop 1 shape)
          slice-dim-bounds [i (inc i)]
          new-strides (long-array (drop-last strides))]
      (if (empty? new-shape)
        ;;rank0 array == scalar value at index i
        (.get-1d distribution i)
        ;;rank - dim+1 array
        (make-arrayspace-matrix
         (:implementation-key api)
         (:multiarray-key api)
         :shape new-shape
         :type (:element-type api)
         :offset (* (nth strides dimension) i)
         :strides (vec new-strides)
         :distribution distribution))))

  PMatrixCloning
  (clone [m]
    (make-arrayspace-matrix
     (:implementation-key api)
     (:multiarray-key api)
     :shape (shape m)
     :type (:element-type api)
     :offset (.offset domain-map)
     :distribution distribution))

  PConversion
  (convert-to-nested-vectors [m]
    (let [eseq (element-seq m)
          s (shape m)]
      (if-not (count s) (vec eseq)
              (loop [countdown (count s) revshapes (reverse s) accum eseq]
                (if (zero? countdown) (first accum)
                    (recur (dec countdown)
                           (rest revshapes)
                         (map vec (partition (first revshapes) accum))))))))
  PCoercion
  (coerce-param [m param]
    (cond
     (is-scalar? param) param
     (instance? ArrayspaceMatrix param) param
     (array? param) (construct-matrix api (convert-to-nested-vectors param))
     :default (construct-matrix api (convert-to-nested-vectors param))))

  PReshaping
  (reshape [m shape] nil)

  PMatrixAdd
  (matrix-add [m a]
    (assert (= (dimensionality m) 2))
    (element-map m + a))
  (matrix-sub [m a]
    (assert (= (dimensionality m) 2))
    (element-map m - a))

  PSummable
  (sum [m]
    (element-reduce m +))

  PMatrixMultiply
  (element-multiply [m a]
    (element-map m * a))
  (matrix-multiply [m a]
    (element-map m * a))

  PMatrixScaling
  (scale [m a]
    (element-map m #(* % a)))
  (pre-scale [m a]
    (element-map m (partial * a))))

;; I would have placed these inline in the body of the deftype,
;; but it barfs with an 'Unsupported Binding Form' error on the variadic method impls
(extend-protocol PFunctionalOperations
  ArrayspaceMatrix
  (element-seq [m]
    ;;XXX-- this is horrible performance-wise, but just trying to get it
    ;;Will create true Arrayspace ISeq/Iterator impls later
    (let [arr (make-array (:element-type (.api m))
                          (element-count-of-shape (seq (shape (.domain m)))))]
      (do-elements-indexed m (fn [idx el] (aset arr idx el)))
      (seq arr)))

  (element-map
    ([m f]
       (map f (element-seq m)))
    ([m f a]
       (map f (element-seq m) (if (scalar? a) (repeat a) a)))
    ([m f a more]
       (apply element-seq f m (if (scalar? a) (repeat a) a) more)))

  (element-map!
    ;;Apply fn to all elements in m, setting that element to the result in-place
    ([m f]
       (do-elements! m f))
    ([m f a]
       (do-elements! m #(f % a)))
    ([m f a more]
       (TODO)))

  (element-reduce
    ([m f]
       (reduce f (element-seq m)))
    ([m f init]
       (reduce f init (element-seq m)))))

;; (extend-protocol PAssignment
;;   ArrayspaceMatrix
;;   (assign!
;;     ([m source] nil))

;;   (assign-array!
;;     ([m arr] nil)
;;     ([m arr start length] nil)))


(defrecord ArrayspaceMatrixApi
    [implementation-key multi-array-key element-type]

  PImplementation
  (implementation-key [m]
    "Returns a keyword representing this implementation.
     Each implementation should have one unique key."
    implementation-key)

  (construct-matrix [m data]
    "Returns a new matrix containing the given data. Data should be in the form
     of either nested sequences or a valid existing matrix"
    (let [vdata (vec (seq data))
          flat-data (vec (flatten vdata))
          data-shape (vec (get-shape vdata))
          - (when (empty? data-shape)(throw (Exception. "shape cannot be empty")))]

      (make-arrayspace-matrix implementation-key multi-array-key
                              :shape data-shape
                              :type element-type
                              :data flat-data)))

  (new-vector [m length]
    "Returns a new vector (1D column matrix) of the given length."
    (make-arrayspace-matrix implementation-key multi-array-key
                            :shape [length]
                            :type element-type))

  (new-matrix [m rows columns]
    "Returns a new matrix (regular 2D matrix) with the given number of rows and
     columns."
    (make-arrayspace-matrix implementation-key multi-array-key
                            :shape [rows columns]
                            :type element-type))

  (new-matrix-nd [m shape]
    "Returns a new general matrix of the given shape.
     Shape must be a sequence of dimension sizes."
    (make-arrayspace-matrix implementation-key multi-array-key
                            :shape shape
                            :type element-type))

  (supports-dimensionality? [m dimensions]
    "Returns true if the implementation supports matrices with the given number
    of dimensions."
    (> dimensions 0)))

(defn- print-arrayspace-matrix
  [m #^java.io.Writer w]
  (.write w "#:ArrayspaceMatrix")
  (.write w "{:domain ")
  (print-method (.domain m) w)
  (.write w ", :domain-map ")
  (print-method (.domain-map m) w)
  (.write w ", :distribution ")
  (print-method (.distribution m) w)
  (.write w "}"))

(defmethod print-method ArrayspaceMatrix [m w]
  (print-arrayspace-matrix m w))

(defn do-elements!
  [m el-fn]
  (do-elements-loop m coords idx el
                    (set-nd m coords (el-fn (.get-nd m coords)))))

(defn do-elements-indexed [m el-fn]
  (do-elements-loop m coords idx el
                    (el-fn idx (.get-nd m coords))))

(defn do-elements [m el-fn]
  (do-elements-indexed m (fn [idx el] (el-fn el))))

(defn map-elements [m el-fn]
  (map el-fn (element-seq m)))


(defn make-arrayspace-matrix
  [impl-kw multi-array-kw & {:keys [shape type data offset strides distribution partition-count]}]
  (let [resolved-type (if data (resolve-type-from-data data) (resolve-type type))
        domain (make-domain multi-array-kw :shape shape)
        distribution (or distribution (make-distribution multi-array-kw
                                                   :type resolved-type
                                                   :element-count (element-count-of-shape shape)
                                                   :partition-count (or partition-count 1)
                                                   :data data))
        domain-map (make-domain-map :default
                                    :domain domain
                                    :distribution distribution
                                    :offset (or offset 0)
                                    :strides strides)
        api (ArrayspaceMatrixApi. impl-kw multi-array-kw resolved-type)]
    (println (format "strides: %s" strides))
    (ArrayspaceMatrix. api domain domain-map distribution resolved-type)))

(def double-local-1d-java-array-impl
  (make-arrayspace-matrix :double-local-1d-java-array
                          :local-1d-java-array
                          :shape [1]
                          :type double))

(def double-local-buffer-impl
  (make-arrayspace-matrix :double-local-buffer
                          :local-byte-buffer
                          :shape [1]
                          :type double))


(def double-partitioned-buffer-impl
  (make-arrayspace-matrix :double-partitioned-buffer
                          :partitioned-byte-buffer
                          :shape [1]
                          :type double))

(def int-local-1d-java-array-impl
  (make-arrayspace-matrix :int-local-1d-java-array
                          :local-1d-java-array
                          :shape [1]
                          :type int))

(def int-local-buffer-impl
  (make-arrayspace-matrix :int-local-buffer
                          :local-byte-buffer
                          :shape [1]
                          :type int))

(def int-partitioned-buffer-impl
  (make-arrayspace-matrix :int-partitioned-buffer
                          :partitioned-byte-buffer
                          :shape [1]
                          :type int))


(imp/register-implementation double-local-1d-java-array-impl)
(imp/register-implementation double-local-buffer-impl)
(imp/register-implementation double-partitioned-buffer-impl)
(imp/register-implementation int-local-1d-java-array-impl)
(imp/register-implementation int-local-buffer-impl)
(imp/register-implementation int-partitioned-buffer-impl)
