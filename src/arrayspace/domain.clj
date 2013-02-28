(ns arrayspace.domain
  (:require
   [clojure.tools.macro :as macro]
   [arrayspace.protocols :refer [Domain DomainMap shape rank]]
   [arrayspace.core :refer [make-domain make-domain-map]]))


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
