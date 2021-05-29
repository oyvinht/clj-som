(ns som.core)

;; Warn on performance hit due to reflection
(set! *warn-on-reflection* true)

(defn euclidean-dist [^"[D" vec-1 ^"[D" vec-2]
  "Euclidean distance between two vectors."
  (Math/sqrt
   (reduce (fn [res i]
             (+ res (Math/pow (- (aget vec-1 i) (aget vec-2 i)) 2)))
           0 (range (alength vec-1)))))

(defn manhattan-dist [^"[D" vec-1 ^"[D" vec-2]
  "Manhattan distance between two vectors."
  (reduce (fn [res i]
            (+ res (Math/abs (- (aget vec-1 i) (aget vec-2 i)))))
          0 (range (alength vec-1))))

(defn make-som [input-size x-size y-size]
  "Create a Self-Organizing Map with random weights."
  {:nodes (to-array-2d
           (reduce
            (fn [rows r-idx]
              (cons (reduce (fn [cols c-idx]
                              (let [a (make-array Double/TYPE input-size)]
                                (dotimes [i input-size]
                                  (aset ^"[D" a i (double (rand 1))))
                                (cons a cols)))
                            []
                            (range y-size))
                    rows))
            []
            (range x-size)))})

(defn- find-bmu [som input-vec diff-fn]
  "Find index of best matching unit."
  (let [nodes ^"[[Ljava.lang.Object;" (:nodes som)]
    (first
     (reduce
      (fn [best r-idx]
        (reduce (fn [best c-idx]
                  (let [diff (diff-fn input-vec (aget nodes r-idx c-idx))]
                    (if (< diff (nth best 1)) ; Better?
                      [[r-idx c-idx] diff]
                      best)))
                best ; Start column loop with best so far
                (range (alength ^"[[Ljava.lang.Object;" (aget nodes r-idx)))))
      [[0 0] (diff-fn input-vec (aget nodes 0 0))] ; 0,0 Is best at start
      (range 1 (alength nodes))))))

