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
  
(defn train [som sample-generator num-iterations dist-fn]
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
              bmu (find-bmu som sample-vec dist-fn)]
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

(defn u-matrix [som dist-fn]
  "Create Unified distance Matrix for SOM."
  (let [nodes ^"[Ljava.lang.Object;" (:nodes som)
        x-size (- (* (alength nodes) 2) 1)
        y-size (- (* (alength ^"[Ljava.lang.Object;" (aget nodes 0)) 2) 1)
        umatrix (make-array Double/TYPE x-size y-size)]
    ;; First pass: Fill in all distances between nodes
    (reduce
     (fn [res y]
       (reduce
        (fn [res x]
          (if (even? y) ; Even y: Dist between left and right of pos
            (let [lx (/ (dec x) 2) rx (inc lx)]
              (aset umatrix x y (dist-fn (aget nodes lx (/ y 2))
                                         (aget nodes rx (/ y 2)))))
            (if (even? x)
              ;; Odd y & even x: Dist between over and under pos
              (let [ux (/ x 2) uy (/ (dec y) 2) ox ux oy (inc uy)]
                (aset umatrix x y (dist-fn (aget nodes ox oy)
                                           (aget nodes ux uy))))
              ;; Odd y & odd x: Mean of diagonals
              (let [ulx (/ (dec x) 2) luy (/ (dec y) 2) ; Under-left
                    orx (inc ulx) ory (inc luy) ; Over-right
                    urx orx ury luy ; Under-right
                    olx ulx oly ory] ; Over-left
                ;;(aset umatrix x y (aget umatrix x y))
                 (aset umatrix x y (/ (+ (dist-fn (aget nodes ulx luy)
                                                  (aget nodes orx ory))
                                         (dist-fn (aget nodes olx oly)
                                                  (aget nodes urx ury)))
                                      2))))))
        nil
        (if (even? y)
          (range 1 x-size 2) ; Skip slots with nodes in
          (range x-size))))
     nil
     (range y-size))
    ;; Second pass: Fill in averages at node positions
    (reduce
      (fn [res y]
        (reduce
         (fn [res x]
           (aset umatrix x y
                 (cond (= y 0)
                       (cond (= x 0) ; Lower left
                             (/ (+ (aget umatrix 0 1)
                                   (aget umatrix 1 1)
                                   (aget umatrix 1 0))
                                3)
                             (= x (dec x-size)) ; Lower right
                             (/ (+ (aget umatrix (dec x) y)
                                   (aget umatrix (dec x) (inc y))
                                   (aget umatrix x (inc y)))
                                3)
                             true
                             (/ (+ (aget umatrix (dec x) y)
                                   (aget umatrix (dec x) (inc y))
                                   (aget umatrix x (inc y))
                                   (aget umatrix (inc x) (inc y))
                                   (aget umatrix (inc x) y))
                                5))
                       (= y (dec y-size))
                       (cond (= x 0) ; Upper left
                             (/ (+ (aget umatrix 0 (dec y))
                                   (aget umatrix 1 (dec y))
                                   (aget umatrix 1 y))
                                3)
                             (= x (dec x-size)) ; Upper right
                             (/ (+ (aget umatrix (dec x) y)
                                   (aget umatrix (dec x) (dec y))
                                   (aget umatrix x (dec y)))
                                3)
                             true
                             (/ (+ (aget umatrix (dec x) y) 
                                   (aget umatrix (dec x) (dec y))
                                   (aget umatrix x (dec y))
                                   (aget umatrix (inc x) (dec y))
                                   (aget umatrix (inc x) y))
                                5))
                       true
                       (cond (= x 0) ; Middle left
                             (/ (+ (aget umatrix x (inc y))
                                   (aget umatrix (inc x) (inc y))
                                   (aget umatrix (inc x) y)
                                   (aget umatrix (inc x) (dec y))
                                   (aget umatrix x (dec y)))
                                5)
                             (= x (dec x-size)) ; Middle right
                             (/ (+ (aget umatrix x (inc y))
                                   (aget umatrix (dec x) (inc y))
                                   (aget umatrix (dec x) y)
                                   (aget umatrix (dec x) (dec y))
                                   (aget umatrix x (dec y)))
                                5)
                             true
                             (/ (+ (aget umatrix x (inc y))
                                   (aget umatrix (inc x) (inc y))
                                   (aget umatrix (inc x) y)
                                   (aget umatrix (inc x) (dec y))
                                   (aget umatrix x (dec y))
                                   (aget umatrix (dec x) (dec y))
                                   (aget umatrix (dec x) y)
                                   (aget umatrix (dec x) (inc y)))
                                8)))))
         res
         (range 0 x-size 2)))
      umatrix
      (range 0 y-size 2))
    ;; Third pass: Normalize to range 0:1
    (let [smallest (reduce (fn [smallest y]
                             (min smallest
                                  (reduce
                                   (fn [smallest x]
                                     (min smallest (aget umatrix x y)))
                                   smallest
                                   (range x-size))))
                           (aget umatrix 0 0)
                           (range y-size))]
      ;; Move all down so that min is at 0
      (doall (for [x (range x-size) y (range y-size)]
               (aset umatrix x y (- (aget umatrix x y) smallest))))
      ;; Squash into 0:1 range
      (let [biggest (reduce (fn [smallest y]
                             (max smallest
                                  (reduce
                                   (fn [smallest x]
                                     (max smallest (aget umatrix x y)))
                                   smallest
                                   (range x-size))))
                            (aget umatrix 0 0)
                            (range y-size))]
        (doall (for [x (range x-size) y (range y-size)]
                 (aset umatrix x y (/ (aget umatrix x y) biggest))))
        umatrix))))

(defn pprint-u-matrix [^"[[D" u-matrix]
  (reduce (fn [r y] ;; Note: Rotate -90 and flip to show x/y correctly
            (reduce (fn [r x]
                      (print (format " %5f" (aget u-matrix x y)))
                      r)
                    r
                    (range (alength u-matrix)))
            (print "\n"))
          u-matrix
          (reverse (range (alength ^"[D" (aget u-matrix 0)))))
  nil)

(defn export-u-matrix [^"[[D" u-matrix ^String png-filename]
  (let [scale 5
        x-size (alength u-matrix)
        y-size (alength ^"[D" (aget u-matrix 0))
        bi (java.awt.image.BufferedImage.
            (* x-size scale)
            (* y-size scale)
            java.awt.image.BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)]
    (reduce (fn [n x]
              (reduce (fn [n y]
                        (.setColor g (new java.awt.Color
                                          (float (aget u-matrix x y))
                                          (float (aget u-matrix x y))
                                          (float (aget u-matrix x y))))
                        (.fillRect g (* x scale) (* y scale) scale scale))
                      nil
                      (range y-size)))
            nil
            (range x-size))
    (javax.imageio.ImageIO/write bi "png" (java.io.File. png-filename))))
