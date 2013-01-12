(ns arrayspace.domain)

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

(defmethod make-domain :default 
  [type-kw  & {:keys [shape]}]
  (let [rank (count shape)
        count (element-count-of-shape shape)
        strides (strides-of-shape shape)]
    (OrdinalDomain. shape rank strides)))

(defmethod make-domain-map :default
  [type-kw  & {:keys [domain distribution]}]
  (LocalBlockDataMap. dom dist))
                 