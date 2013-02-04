(ns arrayspace.domain
  (:require
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

(defn strides-of-shape [shape]
  (reverse (reductions * (reverse (concat (drop 1 shape) [1])))))

(defn element-count-of-shape [shape]
  (reduce * shape))

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
    [^longs shape ^long rank]
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
  (let [rank (count shape)
        count (element-count-of-shape shape)]
    (OrdinalDomain. (long-array shape) rank)))

(defmethod make-domain-map :default
  [type-kw  & {:keys [domain distribution offset strides]}]
  {:pre [(not-any? nil? '(domain distribution))]}
  (LocalBlockDomainMap. domain distribution
                        (or offset 0)
                        (long-array (or strides (strides-of-shape (shape domain))))))
