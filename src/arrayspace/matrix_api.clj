(ns arrayspace.matrix-api
  (:require
   [clojure.tools.macro :as macro]
   [arrayspace.protocols :refer :all]
   [arrayspace.core :refer [make-domain make-domain-map make-distribution]]
   [arrayspace.domain :refer [strides-of-shape element-count-of-shape shape-from-ranges
                              flatten-coords shape-from-ranges]]
   [arrayspace.java-array-utils :refer [adel a== acopy ainc-long]]
   [arrayspace.distributions.contiguous-java-array]
   [arrayspace.distributions.contiguous-buffer]
   [arrayspace.distributions.partitioned-buffer]
   [arrayspace.types :refer [resolve-type resolve-type-from-data sym-typed sym-long sym-int
                             gensym-typed gensym-long gensym-int]]
   [clojure.core.matrix.protocols :refer :all]
   [clojure.core.matrix :refer [scalar? array? ecount]]
   [clojure.core.matrix.implementations :as imp]
   [clojure.core.matrix.impl.wrappers :refer [wrap-scalar]]
   [clojure.core.matrix.impl.persistent-vector]))

(declare make-arrayspace-matrix do-elements do-elements-indexed do-elements!)

(defn TODO []
  ;;(throw (Exception. "TODO- NOT IMPLEMENTED YET"))
  nil)

(defn debug-compare [m o]
  (let [oa (array? o)
        shape-eq (every? true? (map == (get-shape m) (get-shape o)))
        el-eq (every? true? (map == (element-seq m) (element-seq o)))]
    (println (format "EQ: FALSE: %s %s" m o))
    (println (format "o: array? %s, shape-eq: %s el-eq: %s" oa shape-eq el-eq))
    (println (format "m-shape: %s, o-shape: %s" (vec (get-shape m)) (vec (get-shape o))))
    (println (format "m-seq: %s, o-seq: %s" (vec (element-seq m)) (vec (element-seq o))))))

(defmacro do-elements-loop
  [[m coords idx el as ar] & body]
  (let [coords (sym-typed coords longs)
        idx (sym-long idx)
        domain (gensym 'domain)
        bottom-ranges (gensym-typed 'bottom-ranges longs)
        top-ranges (gensym-typed 'top-ranges longs)
        shape (gensym-typed 'shape longs)
        rank (gensym-long 'rank)
        elcount (gensym-long 'elcount)
        last-dim (gensym-int 'last-dim)
        ridx (gensym-typed 'ridx longs)
        dim (gensym-long 'dim)
        argseq (gensym 'argseq)
        args (gensym 'args)
        ]
    (letfn [(inc-last-coords []
              `(aset ~coords ~last-dim
                     (unchecked-inc (aget ~coords ~last-dim))))
            (dim-at-max? [] `(= (aget ~coords (int (aget ~ridx ~dim)))
                                (aget ~top-ranges (aget ~ridx ~dim))))
            (roll-idx []  `(aset ~coords (aget ~ridx ~dim) (aget ~bottom-ranges (aget ~ridx ~dim))))
            (carry-idx [] `(aset ~coords (aget ~ridx (unchecked-inc ~dim))
                                 (unchecked-inc (aget ~coords (int (aget ~ridx (unchecked-inc ~dim)))))))
            (top-dim? [] `(zero? (aget ~ridx ~dim)))]
      `(let [~domain (.domain ~m)
             ~bottom-ranges (:bottom-ranges ~domain)
             ~top-ranges (:top-ranges ~domain)
             ~shape (long-array (shape-from-ranges ~bottom-ranges ~top-ranges))
             ~rank (count ~shape)
             ~elcount (element-count-of-shape ~shape)
             ~coords (long-array ~bottom-ranges)
             ~ridx (long-array (reverse (range ~rank)))
             ~last-dim (aget ~ridx 0)]
         (loop [~idx 0 ~argseq ~as]
           (if (== ~idx ~elcount) nil
               (let [~el (.get-nd ~m ~coords)
                     ~ar (when ~argseq (vec (map first ~argseq)))]
                 ;;(println (format "idx: %s elcount: %s, ~argseq: %s, args: %s" ~idx ~elcount ~argseq ~ar))
                 ~@body
                 ;;(try ~@body (catch Exception ex# (do (clojure.stacktrace/print-stack-trace ex#) (throw ex#))))
                 ~(inc-last-coords)
                 (dotimes [~dim ~rank]
                   (when ~(dim-at-max?)
                     ~(roll-idx) (when-not ~(top-dim?) ~(carry-idx))))
                 (recur (inc ~idx) (if ~argseq (vec (map rest ~argseq)) nil)))))))))

(defn lazy-eseq
  ([m] (lazy-eseq m (long-array (:bottom-ranges (.domain m)))))
  ([m ^longs coords]
     (let [domain (.domain m)
           bottom-ranges (longs (:bottom-ranges domain))
           top-ranges (longs (:top-ranges domain))
           shape (long-array (shape-from-ranges bottom-ranges top-ranges))
           rank (long (count shape))
           ridx (long-array (reverse (range rank)))
           last-dim (long (aget ridx 0))]
       (lazy-eseq m coords bottom-ranges top-ranges rank ridx last-dim)))
  ([m ^longs coords bottom-ranges top-ranges rank ridx last-dim]
     (let [el (.get-nd m coords)
           inc-coords (fn inc-coords []
                        (ainc-long coords last-dim)
                        (dotimes [i rank]
                          (let [dim (aget ridx i)]
                            (when (a== coords top-ranges dim)
                              (acopy coords bottom-ranges dim)
                              (when-not (zero? dim)
                                (ainc-long coords (dec dim)))))) coords)]
             (cons el (if (every? true? (map #(== %1 (dec %2)) coords top-ranges))
                        nil
                        (lazy-seq (lazy-eseq m (inc-coords)
                                             bottom-ranges top-ranges
                                             rank ridx last-dim)))))))
(defn ensure-seq [o max]
  (cond
   (scalar? o) (repeat max o)
   (array? o) (element-seq o)
   :else o))

(defn ensure-seqs [objs max]
  (map #(ensure-seq % max) objs))

(defn lazy-slice-seq
  ([m]
     (lazy-slice-seq m 0 (count m)))
  ([m idx stop]
     (if (= idx stop) nil
         (cons (.get-slice m 0 idx) (lazy-seq (lazy-slice-seq m (inc idx) stop))))))

(deftype ArrayspaceMatrix
    [implementation-key multi-array-key domain domain-map distribution element-type]

  Object
  (equals [m o]
    ;;(debug-compare m o)
    (and (array? o)
         (every? true? (map == (get-shape m) (get-shape o)))
         (every? true? (map == (element-seq m) (element-seq o)))))

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

  java.util.Collection
  (contains [m o] (boolean (some #(= % o) (element-seq m))))
  (containsAll [m c] (every? #(.contains m %) c))
  (isEmpty [m] (zero? (count m)))
  (toArray [m]
    (let [arr (make-array Object ;;element-type
                          (element-count-of-shape (shape (.domain m))))]
      (do-elements-indexed m (fn [idx el] (aset arr idx el)))
      arr))
  (toArray [m arr]
    (if (>= (count arr) (int (count m)))
      (do  (do-elements-indexed m (fn [idx el] (aset arr idx el)))
           arr)
      (let [arr (make-array Object ;;element-type
                            (element-count-of-shape (shape (.domain m))))]
        (do-elements-indexed m (fn [idx el] (aset arr idx el)))
        arr)))
  (size [m] (int (count m)))
  (add [_ o] (throw (UnsupportedOperationException.)))
  (addAll [_ c] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  (^boolean remove [_ o] (throw (UnsupportedOperationException.)))
  (removeAll [_ c] (throw (UnsupportedOperationException.)))
  (retainAll [_ c] (throw (UnsupportedOperationException.)))

  clojure.lang.Sequential
  clojure.lang.Seqable

  (seq [m]
    (lazy-slice-seq m))

  Domain
  (shape [m] (.shape domain))
  (rank [m] (.rank domain))

  PImplementation
  (implementation-key [m] implementation-key)
  (construct-matrix
   [m data]
   (if (instance? ArrayspaceMatrix data) (.clone data)
       (let [vdata (convert-to-nested-vectors data)
             flat-data (vec (flatten vdata))
             data-shape (get-shape vdata)]
         (when (empty? data-shape)
           (throw (Exception.
                   (str "shape cannot be empty: " data ", vdata: " vdata ", data-shape: " data-shape))))
         (make-arrayspace-matrix implementation-key
                                 multi-array-key
                                 :shape data-shape
                                 :element-type element-type
                                 :data flat-data))))

  (new-vector [m length]
    (make-arrayspace-matrix
     implementation-key
     multi-array-key
     :shape [length]
     :element-type element-type))
  (new-matrix [m rows columns]
    (make-arrayspace-matrix
     implementation-key
     multi-array-key
     :shape [rows columns]
     :element-type element-type))
  (new-matrix-nd [m shape]
    (make-arrayspace-matrix
     implementation-key
     multi-array-key
     :shape shape
     :element-type element-type))
  (supports-dimensionality? [m dimensions]
    (> dimensions 0))

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
          (.get-flat distribution (transform-coords domain-map [row])))
  (get-2d [m row column]
          (assert (= (rank domain) 2))
          (.get-flat distribution (transform-coords domain-map [row column])))
  (get-nd [m indexes]
          (.get-flat distribution (transform-coords domain-map indexes)))

  PIndexedSetting
  (set-1d [m row v]
    (assert (= (rank domain) 1))
    (let [mc (.clone m)]
      (set-1d! mc row v)
      mc))
  (set-2d [m row column v]
    (assert (= (rank domain) 2))
    (let [mc (.clone m)]
      (set-2d! mc row column v)
      mc))
  (set-nd [m indexes v]
    (let [mc (.clone m)]
      (set-nd! mc indexes v)
      mc))
  (is-mutable? [m] true)

  PIndexedSettingMutable
  (set-1d! [m row v]
    (assert (= (rank domain) 1))
    (.set-flat! distribution (transform-coords domain-map [row]) v)
    m)
  (set-2d! [m row column v]
    (assert (= (rank domain) 2))
    (.set-flat! distribution (transform-coords domain-map [row column]) v)
    m)
  (set-nd! [m indexes v]
    (.set-flat! distribution (transform-coords domain-map indexes) v)
    m)

  PMatrixSlices
  (get-row [m i]
    (assert (= (rank domain) 2))
    (.get-slice m 0 i))

  (get-column [m i]
    (assert (= (rank domain) 2))
    (.get-slice m 1 i))

  (get-major-slice [m i]
    (.get-slice m 0 i))

  (get-slice [m dimension i]
    {:pre [(and (>= dimension 0) (>= (dec (rank m)) dimension))]}
    (let [new-shape (object-array (drop 1 (shape m)))
          strides (.strides (.domain-map m))
          new-strides (adel strides dimension)]
      (if (empty? new-shape)
        ;;rank0 array == scalar value at index i
        ;;(wrap-scalar (.get-flat distribution i))
        ;;(.get-flat distribution i)
        (.get-flat distribution (transform-coords domain-map [i]))
        ;;rank - dim+1 array
        (let [offset (+ (:offset domain-map) (* (nth strides dimension) i))
              m (make-arrayspace-matrix
                 implementation-key
                 multi-array-key
                 :shape new-shape
                 :element-type element-type
                 :offset offset
                 :strides (vec new-strides)
                 :distribution distribution)]
          m
          ))))

  PMatrixCloning
  (clone [m]
    (make-arrayspace-matrix
     implementation-key
     multi-array-key
     :shape (shape m)
     :element-type element-type
     :offset (.offset domain-map)
     :distribution (.copy distribution)))

  PConversion
  (convert-to-nested-vectors [m]
    ;;(println (format "XXX -- CNV: m: %s" m))
    (try
      (let [eseq (element-seq m)
            s (shape m)]
        ;;if no shape (scalar value) wrap in vec
        ;;XXX--not necessary? Shouldn't get dispatched here if a scalar value
        (if-not (count s) (vec eseq)
                (loop [countdown (count s) revshapes (reverse s) accum eseq]
                  (if (zero? countdown) (first accum)
                      (recur (dec countdown)
                             (rest revshapes)
                             (map vec (partition (first revshapes) accum)))))))
      (catch Exception e
        (do (println (format "XXX -- CNV: m: %s, ex: %s" m e)) nil))))
  PCoercion
  (coerce-param [m param]
    (if (instance? ArrayspaceMatrix param)
      (.clone param)
      (construct-matrix m param)))

  PReshaping
  (reshape [m shape] nil)

  PMatrixAdd
  (matrix-add [m a]
    (element-map m + a))
  (matrix-sub [m a]
    (element-map m - a))

  PSummable
  (element-sum [m]
    (element-reduce m +))

  PMatrixMultiply
  (matrix-multiply [m a]
    (coerce-param m (matrix-multiply
                        (coerce-param [] m)
                        (coerce-param [] a))))
  (element-multiply [m a]
    (element-map m clojure.core/* a))

  ;; (element-multiply [m a]
  ;;   (element-map m * a))
  ;; (matrix-multiply [m a]
  ;;   (element-map m * a))

  PMatrixScaling
  (scale [m a]
    (element-map m #(* % a)))
  (pre-scale [m a]
    (element-map m (partial * a))))

;; I would have placed these inline in the body of the deftype,
;; but it barfs with an 'Unsupported Binding Form' error on the variadic method impls
(extend-protocol PFunctionalOperations
  ArrayspaceMatrix
  (element-seq [m] (lazy-eseq m))

  (element-map
    ;;Maps f over all elements of m (and optionally other matrices), returning a new matrix
    ([m f]
       (let [new-m (.clone m)]
         (do-elements! new-m f)
         new-m))
    ([m f a]
       (let [new-m (.clone m)]
         (do-elements! new-m f a)
         new-m))
    ([m f a more]
       (let [new-m (.clone m)]
         (do-elements! new-m f a more)
         new-m)))

  (element-map!
    ;; Maps f over all elements of m (and optionally other matrices), mutating the elements of m in place.
    ;; Must throw an exception if m is not mutable.
    ([m f]
       (do-elements! m f) m)
    ([m f a]
       (do-elements! m f a) m)
    ([m f a more]
       (do-elements! m f a more) m))

  (element-reduce
    ([m f]
       (reduce f (element-seq m)))
    ([m f init]
       (reduce f init (element-seq m)))))

 (extend-protocol PAssignment
   ArrayspaceMatrix
;;   (assign!
;;     ([m source] nil))

   (assign-array!
     ([m arr]
        (arrayspace.distribution/set-data-flat! (.distribution m) (vec arr)))
     ([m arr start length]
        (arrayspace.distribution/set-data-flat! (.distribution m) (subvec (vec arr) start length)))))

;; generic versions of matrix ops
(extend-protocol PMatrixOps
  ArrayspaceMatrix
    (trace [m]
      (when-not (== 2 (dimensionality m)) (throw (Exception.  "Trace requires a 2D matrix")))
      (let [rc (dimension-count m 0)
            cc (dimension-count m 1)
            dims (long rc)]
        (when-not (== rc cc) (throw (Exception.  "Can't compute trace of non-square matrix")))
        (loop [i 0 res 0.0]
          (if (>= i dims)
            res
            (recur (inc i) (+ res (double (get-2d m i i))))))))
    (negate [m]
      (scale m -1.0))
    (length-squared [m]
      (element-reduce #(+ %1 (* %2 *2)) 0.0 m))
    (length [m]
      (Math/sqrt (length-squared m)))
    (transpose [m]
      (case (long (dimensionality m))
        0 m
        1 m
        2 (coerce-param m (apply mapv vector (map
                                              #(coerce-param [] %)
                                              (get-major-slice-seq m))))

        (let [d (make-domain :default :shape (shape m))
              dm (make-domain-map :transposed
                                  :domain d
                                  :distribution (.distribution m)
                                  :offset (.offset (.domain-map m)))]

          (make-arrayspace-matrix
           (.implementation-key m)
           (.multi-array-key m)
           :element-type (.element-type m)
           :distribution (.distribution m)
           :domain d
           :domain-map dm
           :offset (.offset dm))))))

(defn- print-arrayspace-matrix
  [m #^java.io.Writer w]
  (let [rep {:array (.convert-to-nested-vectors m)
             :shape (vec (.shape m))
             :element-type (.element-type m)}]
      (.write w "#ArrayspaceMatrix")
      (print-method rep w)))

(defmethod print-method ArrayspaceMatrix [m w]
  (print-arrayspace-matrix m w))

(defn do-elements!
  ([m el-fn]
     (let [as nil]
       (do-elements-loop [m coords idx el as a]
                         (.set-nd! m coords (el-fn el)))))
  ([m el-fn a]
     (let [as (ensure-seqs [a] (ecount m))]
       (do-elements-loop [m coords idx el as a]
                         (.set-nd! m coords (apply el-fn el a)))))
  ([m el-fn a & more]
     (let [as (ensure-seqs (cons a more) (ecount m))]
       (do-elements-loop [m coords idx el as a]
                         (.set-nd! m coords (apply el-fn el a))))))

(defn do-elements-indexed [m el-fn]
  (let [as nil]
    (do-elements-loop [m coords idx el as a] (el-fn idx el))))

(defn do-elements [m el-fn]
  (do-elements-indexed m (fn [_ el] (el-fn el))))

(defn map-elements [m el-fn]
  (map el-fn (element-seq m)))

(defn make-arrayspace-matrix
  [impl-kw multi-array-kw & {:keys [shape element-type data offset strides domain domain-map distribution partition-count] :as arrkw}]
  ;;{:pre [(if data (>= (element-count-of-shape shape) (clojure.core.matrix/shape data)) true)]}
  ;;(println (format "make-arrayspace-matrix offset: %s" offset))
  ;;(println (format "make-arrayspace-matrix kw: %s" arrkw))
  (when data
    (when-not (>= (element-count-of-shape shape) (ecount data))
      (println (format "XXX--ecount shape NOT >= shape of data! %s %s %s" shape (element-count-of-shape shape) (ecount data)))))
  (let [resolved-type (if data (resolve-type-from-data data) (resolve-type element-type))
        domain (or domain (make-domain multi-array-kw :shape shape))
        distribution (or distribution (make-distribution multi-array-kw
                                                   :element-type resolved-type
                                                   :element-count (element-count-of-shape (.shape domain))
                                                   :partition-count (or partition-count 1)
                                                   :data data))
        domain-map (or domain-map (make-domain-map :default
                                    :domain domain
                                    :distribution distribution
                                    :offset (or offset 0)
                                    :strides strides))]
    (when-not (instance? Class resolved-type)
      (println (format "XXX-- resolved-type: %s, type: %s" resolved-type (type resolved-type)))
      (throw (IllegalArgumentException. (str "param resolved-type not class: " resolved-type ", type: " (type resolved-type)))))

    (ArrayspaceMatrix. impl-kw multi-array-kw domain domain-map distribution resolved-type)))

(def double-local-1d-java-array-impl
  (make-arrayspace-matrix :double-local-1d-java-array
                          :local-1d-java-array
                          :shape [3 3 3]
                          :element-type Double/TYPE))

(def double-local-buffer-impl
  (make-arrayspace-matrix :double-local-buffer
                          :local-byte-buffer
                          :shape [3 3 3]
                          :element-type Double/TYPE))


(def double-partitioned-buffer-impl
  (make-arrayspace-matrix :double-partitioned-buffer
                          :partitioned-byte-buffer
                          :shape [3 3 3]
                          :element-type Double/TYPE))

(def int-local-1d-java-array-impl
  (make-arrayspace-matrix :int-local-1d-java-array
                          :local-1d-java-array
                          :shape [3 3 3]
                          :element-type Integer/TYPE))

(def int-local-buffer-impl
  (make-arrayspace-matrix :int-local-buffer
                          :local-byte-buffer
                          :shape [3 3 3]
                          :element-type Integer/TYPE))

(def int-partitioned-buffer-impl
  (make-arrayspace-matrix :int-partitioned-buffer
                          :partitioned-byte-buffer
                          :shape [3 3 3]
                          :element-type Integer/TYPE))


(imp/register-implementation double-local-1d-java-array-impl)
(imp/register-implementation double-local-buffer-impl)
(imp/register-implementation double-partitioned-buffer-impl)
(imp/register-implementation int-local-1d-java-array-impl)
(imp/register-implementation int-local-buffer-impl)
(imp/register-implementation int-partitioned-buffer-impl)

(comment
  "Add scratchpad/REPL forms here")
