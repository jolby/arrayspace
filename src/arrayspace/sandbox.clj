(defn mk-world []
  (def ^:dynamic *d1d* [1 2 3 4 5 6 7 8 9
                         10 11 12 13 14 15 16 17 18
                         19 20 21 22 23 24 25 26 27])
  (def ^:dynamic *d2d* [[1 2 3][4 5 6][7 8 9]])
  (def ^:dynamic *d3d* [[[1 2 3]
              [4 5 6]
              [7 8 9]]
             [[10 11 12]
              [13 14 15]
              [16 17 18]]
             [[19 20 21]
              [22 23 24]
              [25 26 27]]])
  (def ^:dynamic *m1* (clojure.core.matrix/matrix arrayspace.matrix-api/int-local-buffer-impl *d1d*))
  (def ^:dynamic *m2* (clojure.core.matrix/matrix arrayspace.matrix-api/int-local-buffer-impl *d2d*))
  (def ^:dynamic *m3* (clojure.core.matrix/matrix arrayspace.matrix-api/int-local-buffer-impl *d3d*)))


(deftype ArrayspaceMatrixSeq
    [array ^Long idx]

  clojure.lang.IPersistentCollection
  (empty [this] ())
  (cons [this o] (clojure.lang.Cons o this))
  (equiv [this o] (.equals array o))

  java.util.Collection
  (contains [this o] (boolean (some #(= % o) this)))
  (containsAll [this c] (every? #(.contains this %) c))
  (isEmpty [_] (zero? (int (- (count array) idx))))
  (toArray [this] (into-array Object this))
  (toArray [this arr]
    (if (>= (count arr) (int (- (count array) idx)))
      (do
        (dotimes [i (int (- (count array) idx))]
          (aset arr i (.nth array i)))
        arr)
      (into-array Object this)))
  (size [_] (int (- (count array) idx)))
  (add [_ o] (throw (UnsupportedOperationException.)))
  (addAll [_ c] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  (^boolean remove [_ o] (throw (UnsupportedOperationException.)))
  (removeAll [_ c] (throw (UnsupportedOperationException.)))
  (retainAll [_ c] (throw (UnsupportedOperationException.)))


  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [this] this)

  clojure.lang.ISeq
  (first [this] (.get-slice array 0 idx))
  (next [this] (if (>= idx (.count array)) nil
                   (ArrayspaceMatrixSeq. array (inc idx))))
  (more [this] (if (>= idx (.count array)) ()
                   (ArrayspaceMatrixSeq. array (inc idx))))

  clojure.lang.IndexedSeq
  (index [this] idx)

  clojure.lang.Counted
  (count [this]
    (int (- (count array) idx))))


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

(defn myscale []
  (let [clonem (clone user/*m3*)]
    (println "M3 BEFORE SCALE!")
    (println (eseq user/*m3*))
    (println "CLONE BEFORE SCALE!")
    (println (eseq clonem))
    (scale! clonem 2)
    (println "CLONE AFTER SCALE!")
    (println (eseq clonem))
    (println clonem)
    (println "RESULT M3 SCALE")
    (println (scale user/*m3* 2))
    (println "M3 AFTER SCALE")
    (println (eseq user/*m3*))
    (equals clonem (scale user/*m3* 2))))

(let [a (repeat 5 1)
      ss [(repeat 5 2) (repeat 5 3) (repeat 5 4)]
      a-ss (vec (cons a ss))
      f +
      mfn (fn mfn [i args]
            (if (= i 0) nil
                (cons (apply f (map first args))
                      (lazy-seq (mfn (dec i) (map rest args))))))]
  (mfn 5 a-ss))

