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

(defn- improved-vec [target-vec train-vec learning-rate distance-factor]
  "Create a new target-vec closer to train-vec."
  (let [v (make-array Double/TYPE (alength ^"[D" target-vec))]
    (doall (for [i (range (alength ^"[D" target-vec))]
             (aset ^"[D" v i (+ (aget ^"[D" target-vec i)
                                (* distance-factor
                                   learning-rate
                                   (- (aget ^"[D" train-vec i)
                                      (aget ^"[D" target-vec i)))))))
    v))
  
(defn train [som sample-generator num-iterations]
  "Train SOM."
  (let [new-nodes (aclone  ^"[[Ljava.lang.Object;" (:nodes som))
        x-size (alength new-nodes)
        y-size (alength ^"[[Ljava.lang.Object;" (aget new-nodes 0))
        map-radius (/ (max x-size y-size) 2)
        time-constant (/ num-iterations (Math/log map-radius))
        calc-radius (fn [iteration]
                      (* map-radius
                         (Math/exp (- (/ iteration time-constant)))))
        calc-learning-rate (fn [iteration]
                             (* 0.1 ; TODO: start-learn-rate
                                (Math/exp (- (/ iteration num-iterations)))))]
    (assoc
     som :nodes
     (reduce
      (fn [nodes iteration]
        (let [radius (calc-radius iteration)
              sample-vec (sample-generator)
              bmu (find-bmu som sample-vec euclidean-dist)]
          ;; TODO: Don't go through everything - only neighborhood
          (reduce
           (fn [nodes r-idx]
             (reduce
              (fn [nodes c-idx]
                (let [distance (Math/sqrt
                                (+ (Math/pow (- r-idx (first bmu)) 2)
                                   (Math/pow (- c-idx (second bmu)) 2)))]
                  (when (< distance radius)
                    (aset nodes r-idx c-idx
                          (improved-vec (aget nodes r-idx c-idx)
                                        sample-vec
                                        (calc-learning-rate iteration)
                                        (Math/exp (- ; Distance factor
                                                   (/
                                                    (Math/pow distance 2)
                                                    (* radius 2)))))))
                  nodes))
              nodes
              (range y-size)))
           nodes
           (range x-size))))
      new-nodes
      (range num-iterations)))))

