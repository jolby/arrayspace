(ns arrayspace.matrix-api
  (:require
   [clojure.tools.macro :as macro]
   [arrayspace.protocols :refer :all :exclude [get-1d]]
   [arrayspace.core :refer [make-domain make-domain-map make-distribution]]
   [arrayspace.domain :refer [strides-of-shape element-count-of-shape flatten-coords]]
   [arrayspace.distributions.contiguous-java-array]
   [arrayspace.distributions.contiguous-buffer]
   [arrayspace.distributions.partitioned-buffer]
   [arrayspace.types :refer [resolve-type]]
   [core.matrix.protocols :refer :all]
   [core.matrix.implementations :as imp]
   [core.matrix.impl.persistent-vector]))

(declare make-arrayspace-matrix)
(declare elwise-fn)

(defn TODO []
  ;;(throw (Exception. "TODO- NOT IMPLEMENTED YET"))
  nil)

(defn slice-3 [m dim i] )


(deftype ArrayspaceMatrix
    [api domain domain-map distribution]

  clojure.lang.Counted
  (count [m]
    (reduce * 1 (shape m)))

  clojure.lang.Indexed
  (nth [m i] (.get-1d distribution i))

  clojure.lang.Sequential

  clojure.lang.Seqable
  (seq [m]
    ;;(map #(.get-1d distribution %1) (range (reduce * 1 (shape m))))
    (let [arr (make-array (:element-type api) (element-count-of-shape (shape domain)))]
      (elwise-fn m (fn [idx el] (aset arr idx el)))
      (seq arr)))

  Domain
  (shape [this] (shape domain))
  (rank [this] (rank domain))

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
  (get-shape [m] (shape domain))
  (is-scalar? [m] false)
  (is-vector? [m] (= 1 (rank domain)))
  (dimension-count [m dimension-number]
    (nth (shape domain) dimension-number))

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

  PMatrixCloning
  (clone [m] (TODO))

  PMatrixSlices
  (get-row [m i]
    (assert (= (rank domain) 2))
    (get-slice m 0 i))

  (get-column [m i]
    ;;XXX--TODO create col-major domain/domain map over this distribution
    (assert (= (rank domain) 2))
    (map #(.get-1d distribution
                   (transform-coords domain-map [%1 i]))
         (range (dimension-count m 0))))

  (get-major-slice [m i]
    (get-slice m 0 i))

  (get-slice [m dimension i]
    {:pre [(and (>= dimension 0) (>= (dec (rank m)) dimension))]}
    (let [shape (vec (shape m))
          strides (strides-of-shape shape)
          new-shape (drop (inc dimension) shape)]
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
         :distribution distribution)))))

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


(defn elwise-fn [m fn]
  (let [shape (int-array (shape m))
        rank (rank m)
        coords (int-array rank)
        elcount (element-count-of-shape shape)
        ridx (int-array (reverse (range rank)))
        last-idx (aget ridx 0)]
    (macro/macrolet
     ;; The variable capture is intentional
     [(inc-last-coords [] `(aset ~'coords ~'last-idx
                                 (inc (aget ~'coords ~'last-idx))))
      (dim-at-max [] `(= (aget ~'coords (aget ~'ridx ~'dim))
                         (aget ~'shape (aget ~'ridx ~'dim))))
      (roll-idx []  `(aset ~'coords (aget ~'ridx ~'dim) 0))
      (carry-idx [] `(aset ~'coords (aget ~'ridx (inc ~'dim))
                        (inc (aget ~'coords (aget ~'ridx (inc ~'dim))))))
      (not-top-dim [] `(> (aget ~'ridx ~'dim) 0))]

     (dotimes [idx elcount]
       ;;(println (format "idx: %2d, coords: %s" idx (vec coords)))
       (fn idx (.get-nd m (vec coords)))
       (inc-last-coords)
       (dotimes [dim rank]
         (when (dim-at-max)
           (roll-idx) (when (not-top-dim) (carry-idx))))))))

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
    ;;(println (format "data: %s" data))
    (let [vdata (vec (seq data))
          flat-data (vec (flatten vdata))
          data-shape (vec (get-shape vdata))
          ;;_ (println (format "shape: %s, data: %s" data-shape vdata))
          - (when (empty? data-shape)(throw (Exception. "shape cannot be empty")))
          matrix
          (make-arrayspace-matrix implementation-key multi-array-key
                                  :shape data-shape
                                  :type element-type
                                  :data flat-data)]
      matrix))

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


(defn make-arrayspace-matrix
  [impl-kw type-kw & {:keys [shape type data offset distribution]}]
  (let [domain (make-domain :type-kw :shape shape)
        distribution (or distribution (make-distribution type-kw
                                                   :type (resolve-type type)
                                                   :element-count (element-count-of-shape shape)
                                                   ;;XXX-- partition-count this should come from
                                                   ;;dynamic var or config param
                                                   ;;:partition-count (count shape)
                                                   :partition-count 1
                                                   :data (if distribution nil data)))
        domain-map (make-domain-map :default
                                    :domain domain
                                    :distribution distribution
                                    :offset (or offset 0))
        api (ArrayspaceMatrixApi. impl-kw type-kw (resolve-type type))]
    (ArrayspaceMatrix. api domain domain-map distribution)))

;; (def double-local-1d-java-array-impl
;;   (ArrayspaceMatrixApi. :double-local-1d-java-array :local-1d-java-array double))

;; (def double-local-buffer-impl
;;   (ArrayspaceMatrixApi. :double-local-buffer :local-byte-buffer double))

;; (def double-partitioned-buffer-impl
;;   (ArrayspaceMatrixApi. :double-partitioned-buffer :partitioned-byte-buffer double))

;; (def int-local-1d-java-array-impl
;;   (ArrayspaceMatrixApi. :int-local-1d-java-array :local-1d-java-array int))

;; (def int-local-buffer-impl
;;   (ArrayspaceMatrixApi. :int-local-buffer :local-byte-buffer int))

;; (def int-partitioned-buffer-impl
;;   (ArrayspaceMatrixApi. :int-partitioned-buffer :partitioned-byte-buffer int))

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
