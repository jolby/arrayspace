(ns dataspace.core)

(defn mget 
  "Gets a value from an array at a specified position. Supports any number of dimensions."
  ([m x]
    (.mget m (if (or (seq? x) (vector? x)) x [x])))
  ([m x y]
    (.mget m [x y]))
  ([m x y & more]
    (.mget m (cons x (cons y more)))))

(defn mset 
  "Sets a value from an array at a specified position. Supports any number of dimensions."
  ([m x val]
    (mset m [x] val))
  ([m x y val]
    (mset m [x y] val))
  ;;XXX--needs more thought
  ;;([m x y & more]
  ;;  (mset m (cons x (cons y more)) val))
  )

(defmulti make-domain
  "Domain construction. Implementors define methods on this for each
  Domain type"
  (fn [type & kwargs] type))

(defmulti make-domain-map
  "DomainMap construction. Implementors define methods on this for each
  DomainMap type"
  (fn [type & kwargs] type))

(defmulti make-distribution
  "Distribution construction. Implementors define methods on this for each
  Distribution/Locale type"
  (fn [type & kwargs] type))

(defmulti make-multi-array
  "MultiArray construction. Implementors define methods on this for each
  MultiArray type"
  (fn [type & kwargs] type))


