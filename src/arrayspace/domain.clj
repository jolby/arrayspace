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

(defn strides-of-shape [shape]
  (reverse (reductions * (reverse (concat (drop 1 shape) [1])))))

(defn element-count-of-shape [shape]
  (reduce * shape))

(defn shape-from-ranges [bottom-ranges top-ranges]
  (map - top-ranges bottom-ranges))

(defn coord-seq [bottom-ranges top-ranges]
    (let [shape (int-array (shape-from-ranges bottom-ranges top-ranges))
          rank (count shape)
          elcount (element-count-of-shape shape)
          out-coords (object-array elcount)
          coords (int-array bottom-ranges)
          ridx (int-array (reverse (range rank)))
          last-dim (aget ridx 0)]
    (macro/macrolet
     ;; The variable capture is intentional
     [(inc-last-coords [] `(aset ~'coords ~'last-dim
                                 (inc (aget ~'coords ~'last-dim))))
      (dim-at-max? [] `(= (aget ~'coords (aget ~'ridx ~'dim))
                          (aget ~'top-ranges (aget ~'ridx ~'dim))))
      (roll-idx []  `(aset ~'coords (aget ~'ridx ~'dim) (aget ~'bottom-ranges (aget ~'ridx ~'dim))))
      (carry-idx [] `(aset ~'coords (aget ~'ridx (inc ~'dim))
                           (inc (aget ~'coords (aget ~'ridx (inc ~'dim))))))
      (top-dim? [] `(zero? (aget ~'ridx ~'dim)))]
     (println (format "shape: %s elcount: %s" (vec shape) elcount))
     (dotimes [idx elcount]
       (println (format "coords: %s" (vec coords)))
       (aset out-coords idx (vec coords))
       (inc-last-coords)
       (dotimes [dim rank]
         (when (dim-at-max?)
             (roll-idx) (when-not (top-dim?) (carry-idx))))))
    (seq out-coords)))


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
    [domain distribution offset strides pinned-dims]
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
  [type-kw  & {:keys [domain distribution offset strides pinned-dims]}]
  {:pre [(not-any? nil? '(domain distribution))]}
  (LocalBlockDomainMap. domain distribution
                        (or offset 0)
                        (long-array (or strides (strides-of-shape (shape domain))))
                        pinned-dims))
