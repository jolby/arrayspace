(ns arrayspace.matrix-api
  (:require
   [arrayspace.protocols :refer :all :exclude [get-1d]]
   [arrayspace.core :refer [make-domain make-domain-map make-distribution]]
   [arrayspace.domain :refer [element-count-of-shape]]
   [arrayspace.distributions.contiguous-java-array]
   [arrayspace.distributions.contiguous-buffer]
   [arrayspace.distributions.partitioned-buffer]
   [core.matrix.protocols :refer :all]
   [core.matrix.implementations :as imp]))

;;(declare ArrayspaceApi)
;;(declare ArrayspaceMatrix)

(declare make-arrayspace-matrix)

(defn TODO []
  ;;(throw (Exception. "TODO- NOT IMPLEMENTED YET"))
  nil)

(deftype ArrayspaceMatrix
    [implementation-key multi-array-key element-type domain domain-map distribution]

  ;; XXX--these are wrong (I think)
  ;; they assume reduction to 1d
  clojure.lang.Counted
  (count [m]
    (reduce * 1 (.shape m)))

  clojure.lang.Indexed
  (nth [m i]
    (.get-1d distribution i))

  clojure.lang.Sequential

  clojure.lang.Seqable
  (seq [m]
    (map #(.get-1d distribution %1) (range (reduce * 1 (.shape m)))))
  ;;(let [n (aget #^ints (:shape descr) 0)]
  ;;  (map (partial slice this 0) (range 0 n))))

  ;; java.lang.Iterable
  ;; (iterator [this]
  ;;   (descr/assert-rank-not-0 descr)
  ;;   (let [n (aget #^ints (:shape descr) 0)]
  ;;     (si/make-dim0-iterator 0 n (partial slice this 0))))



  PImplementation
  (implementation-key [m]
    "Returns a keyword representing this implementation.
     Each implementation should have one unique key."
    implementation-key)
  (construct-matrix [m data]
    "Returns a new matrix containing the given data. Data should be in the form
of either nested sequences or a valid existing matrix"
    (let [flat-data (vec (flatten data))
          data-shape (get-shape data)
          matrix
          (make-arrayspace-matrix implementation-key
                                  multi-array-key
                                  :shape data-shape
                                  :type element-type
                                  :data flat-data)]
      ;; (println (format "Got flat-data: %s" flat-data))
      ;; (println "Got matrix: ")
      ;; (println "API Implementation: ")
      ;; (println m)
      ;; (println "Got matrix: " matrix)
      ;; (println matrix)
      ;; (println "Got matrix DATA: ")
      ;; (println (vec (:array (:distribution matrix))))
      matrix))
  (new-vector [m length]
    "Returns a new vector (1D column matrix) of the given length."
    (make-arrayspace-matrix multi-array-key
                            :shape [length]
                            :type element-type))
  (new-matrix [m rows columns]
    "Returns a new matrix (regular 2D matrix) with the given number of rows and
    columns."
    (make-arrayspace-matrix multi-array-key
                            :shape [rows columns]
                            :type element-type))
  (new-matrix-nd [m shape]
    "Returns a new general matrix of the given shape.
     Shape must be a sequence of dimension sizes."
    (make-arrayspace-matrix multi-array-key
                            :shape shape
                            :type element-type))
  (supports-dimensionality? [m dimensions]
    "Returns true if the implementation supports matrices with the given number
    of dimensions." true)


  Domain
  (shape [this] (.shape domain))
  (rank [this] (.rank domain))

  ;; IndexedAccess
  ;; (mget [this idxs]
  ;;   (.get-1d distribution (.transform-coords domain-map idxs)))

  IndexedMutation
  (mset! [this idxs val]
    (.set-1d! distribution (.transform-coords domain-map idxs) val))

  PDimensionInfo
  (dimensionality [m]
    "Returns the number of dimensions of a matrix"
    (.rank domain))
  (get-shape [m]
    "Returns the shape of the matrix, as an array or sequence of dimension
sizes"
    (.shape domain))
  (is-scalar? [m]
    "Tests whether an object is a scalar value"
    false)
  (is-vector? [m]
    "Tests whether an object is a vector (1D matrix)"
    (= 1 (.rank domain)))
  (dimension-count [m dimension-number]
    "Returns the size of a specific dimension "
    (nth (.shape domain) dimension-number))

  PIndexedAccess
  (get-1d [m row]
          (assert (= (.rank domain) 1))
          (.get-1d distribution row))
  (get-2d [m row column]
          (assert (= (.rank domain) 2))
          (.get-1d distribution (.transform-coords domain-map [row column])))
  (get-nd [m indexes]
          (.get-1d distribution (.transform-coords domain-map indexes)))

  PIndexedSetting
  (set-1d [m row v]
          (assert (= (.rank domain) 1))
          (.set-1d! distribution row v))
  (set-2d [m row column v]
          (assert (= (.rank domain) 2))
          (.set-1d! distribution (.transform-coords domain-map [row column]) v))
  (set-nd [m indexes v]
          (.set-1d! distribution (.transform-coords domain-map) indexes v))
  (is-mutable? [m]
               true)
  PMatrixSlices
  (get-row [m i]
    (TODO))
  (get-column [m i]
    (TODO))
  (get-major-slice [m i]
    (TODO))
  (get-slice [m dimension i]
    (TODO))

  ;; (let [i (long i)
  ;;       dimension (long dimension)]
  ;;   (if (== dimension 0)
  ;;     (mp/get-major-slice m i)
  ;;     (mapv #(mp/get-slice % (dec dimension) i) m)))))


(defn make-arrayspace-matrix
  [impl-kw type-kw & {:keys [shape type data]}]
  (let [domain (make-domain :type-kw :shape shape)
        distribution (make-distribution type-kw
                                :type type
                                :element-count (element-count-of-shape shape)
                                ;;XXX-- partition-count this should come from
                                ;;dynamic var or config param
                                :partition-count (count shape)
                                :data data)
        domain-map (make-domain-map :default
                                    :domain domain
                                    :distribution distribution)]
    (ArrayspaceMatrix. impl-kw type-kw type domain domain-map distribution)))


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
    (let [flat-data (vec (flatten data))
          data-shape (get-shape data)
          matrix
          (make-arrayspace-matrix multi-array-key
                                  :shape data-shape
                                  :type element-type
                                  :data flat-data)]
      ;; (println (format "Got flat-data: %s" flat-data))
      ;; (println "Got matrix: ")
      ;; (println "API Implementation: ")
      ;; (println m)
      ;; (println "Got matrix: " matrix)
      ;; (println matrix)
      ;; (println "Got matrix DATA: ")
      ;; (println (vec (:array (:distribution matrix))))
      matrix))

  (new-vector [m length]
    "Returns a new vector (1D column matrix) of the given length."
    (make-arrayspace-matrix multi-array-key
                            :shape [length]
                            :type element-type))

  (new-matrix [m rows columns]
    "Returns a new matrix (regular 2D matrix) with the given number of rows and
    columns."
    (make-arrayspace-matrix multi-array-key
                            :shape [rows columns]
                            :type element-type))

  (new-matrix-nd [m shape]
    "Returns a new general matrix of the given shape.
     Shape must be a sequence of dimension sizes."
    (make-arrayspace-matrix multi-array-key
                            :shape shape
                            :type element-type))

  (supports-dimensionality? [m dimensions]
    "Returns true if the implementation supports matrices with the given number
    of dimensions." true))

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
