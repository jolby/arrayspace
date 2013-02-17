(ns arrayspace.domain
  (:require
   [clojure.tools.macro :as macro]
   [arrayspace.protocols :refer [Domain DomainMap shape rank]]
   [arrayspace.core :refer [make-domain make-domain-map]]
   [arrayspace.types :refer [sym-typed sym-long sym-int
                             gensym-typed gensym-long gensym-int]]))


(defn valid-coords? [coords shape]
  (and (= (count coords) (count shape))
       (every? (fn [[k v]] (and (>= k 0) (< k v)))
               (map vector coords shape))))

(defn check-valid-coords [coords shape]
  (when-not (valid-coords? coords shape)
    (throw (Exception. (str "Coordinates " (vec coords)
			    " not valid for shape: " (vec shape))))))

(defn normalize-shape [shape]
  (long-array (map #(if (vector? %) (- (nth % 1) (nth % 0)) %) shape)))

(defn strides-of-shape [shape]
  (reverse (reductions * (reverse (concat (drop 1 shape) [1])))))

(defn element-count-of-shape [shape]
  (reduce * (map #(if (vector? %) (- (nth % 1) (nth % 0)) %) shape)))

(defn shape-from-ranges [bottom-ranges top-ranges]
  (long-array (map - top-ranges bottom-ranges)))

(defn bottom-ranges-from-shape [shape]
  (long-array (map #(if (vector? %) (nth % 0) 0) shape)))

(defn top-ranges-from-shape [shape]
  (long-array (map #(if (vector? %) (nth % 1) %) shape)))

(defmacro do-elements-loop
  [m coords idx el & body]
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
        dim (gensym-long 'dim)]
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
             ~shape (long-array (arrayspace.domain/shape-from-ranges ~bottom-ranges ~top-ranges))
             ~rank (count ~shape)
             ~elcount (arrayspace.domain/element-count-of-shape ~shape)
             ~coords (long-array ~bottom-ranges)
             ~ridx (long-array (reverse (range ~rank)))
             ~last-dim (aget ~ridx 0)]
         (dotimes [~idx ~elcount]
           (let [~el (.get-nd ~m ~coords)]
             ~@body
             ~(inc-last-coords)
             (dotimes [~dim ~rank]
               (when ~(dim-at-max?)
                 ~(roll-idx) (when-not ~(top-dim?) ~(carry-idx))))))))))

(defn flatten-coords [coords shape offset]
  "Flatten the coordinates in a multidimensional space to 1d"
  (check-valid-coords coords shape)
  (let [strides (strides-of-shape shape)]
    (+ offset (reduce + (map * coords strides)))))

(defn flatten-coords-array ^long [^longs coords ^longs shape ^longs strides ^long offset]
  "Flatten the coordinates represented as long array in a multidimensional
space to 1d"
  (check-valid-coords coords shape)
  (+ offset (reduce + (map * coords strides))))

(defrecord OrdinalDomain
    [^longs shape ^long rank ^longs bottom-ranges ^longs top-ranges]
  Domain
  (shape ^longs [this] shape)
  (rank ^long [this] rank))

(defrecord LocalBlockDomainMap
    [domain distribution offset strides]
  DomainMap
  (transform-coords [this coords]
    (flatten-coords-array coords (shape domain) strides offset))
  (strides [this] strides)
  (offset [this] offset))

(defmethod make-domain :default
  [type-kw  & {:keys [shape]}]
  (let [nshape (normalize-shape shape)
        rank (count nshape)
        count (element-count-of-shape nshape)
        bottom-ranges (bottom-ranges-from-shape shape)
        top-ranges (top-ranges-from-shape shape)]
    (OrdinalDomain. (long-array nshape) rank bottom-ranges top-ranges)))

(defmethod make-domain-map :default
  [type-kw  & {:keys [domain distribution offset strides]}]
  {:pre [(not-any? nil? '(domain distribution))]}
  (LocalBlockDomainMap. domain distribution
                        (or offset 0)
                        (long-array (or strides (strides-of-shape (shape domain))))))

(defn- print-ordinal-domain
  [d #^java.io.Writer w]
  (.write w "#:OrdinalDomain")
  (.write w "{:shape ")
  (print-method (vec (.shape d)) w)
  (.write w ", :rank ")
  (print-method (.rank d) w)
  (.write w ", :bottom-ranges ")
  (print-method (vec (.bottom-ranges d)) w)
  (.write w ", :top-ranges ")
  (print-method (vec (.top-ranges d)) w)
  (.write w "}"))

(defmethod print-method OrdinalDomain [d w]
  (print-ordinal-domain d w))

(defn- print-domain-map
  [d #^java.io.Writer w]
  (.write w "#:DomainMap")
  (.write w "{:offset ")
  (print-method (.offset d) w)
  (.write w ", :strides ")
  (print-method (vec (.strides d)) w)
  (.write w ", :distribution ")
  (print-method (.distribution d) w)
  (.write w "}"))

(defmethod print-method LocalBlockDomainMap [d w]
  (print-domain-map d w))
