(ns arrayspace.java-array-utils)

(defmacro aloop
  "Execute body with the integer loop-index running from start to end."
  [[loop-index start end] & body]
  ;;Pulled from clj-multiarray project
  `(let [end# (int ~end)]
     (loop [~loop-index (int ~start)]
       (if (< ~loop-index end#)
	 (do ~@body
	     (recur (unchecked-inc ~loop-index)))
	 nil))))

(defn adel
  "Return copy of array a with item at index removed"
  [^longs a ^Integer index]
  ;;Pulled from clj-multiarray project
  (let [n  (alength a)
	c  (int-array (dec n))]
    (aloop [i 0 index]
      (aset c i (aget a i)))
    (aloop [i (inc index) n]
      (aset c (dec i) (aget a i)))
    c))

(defmacro afill
  "Set the elements of array (a 1D Java array) to expr in which
   loop-index has been replaced by the index of the element being set."
  [array loop-index expr]
  ;;Pulled from clj-multiarray project
  `(let [array# ~array
         n# (int (alength array#))]
     (aloop [~loop-index 0 n#]
       (aset array# ~loop-index ~expr))))

(defn a==
  "Test if elements at index are equal"
  [idx a b]
  (== (aget a idx) (aget b idx)))

(defn ainc-long
  "increment the element at idx"
  {:tag long
   :static true
   :inline-arities #{2}
   :inline (fn [arr idx] `(aset ~arr ~idx (unchecked-inc (aget ~arr ~idx))))}
  [^longs arr ^long idx]
  (aset arr idx (unchecked-inc (aget arr idx))))

(defn adec-long
  "increment the element at idx"
  {:tag long
   :static true
   :inline-arities #{2}
   :inline (fn [arr idx] `(aset ~arr ~idx (unchecked-dec (aget ~arr ~idx))))}
  [^longs arr ^long idx]
  (aset arr idx (unchecked-dec (aget arr idx))))

(defn acopy
  "Copy elements from b into a at provided indicies.
When given a range, the indices are inclusive. Return a."
  ([a b idx]
     (aset a idx (aget b idx)) a)
  ([a b start end]
     (System/arraycopy b start a start (- (inc end) start)) a))
