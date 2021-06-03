(ns som.core)

;; Warn on performance hit due to reflection
;;(set! *warn-on-reflection* true)

;;;;---------------------------------------------------------------------------
;;;; Vector distance funtions
;;;;---------------------------------------------------------------------------
(defn euclidean-dist [^doubles vec-1 ^doubles vec-2]
  "Euclidean distance between two vectors (that should have equal length)."
  (Math/sqrt
   (reduce (fn [sum i]
             (+ sum (Math/pow (- (aget vec-1 i) (aget vec-2 i)) 2)))
           0.0 (range (alength vec-1)))))

(defn manhattan-dist [^doubles vec-1 ^doubles vec-2]
  "Manhattan distance between two vectors (that should have equal length)."
  (reduce (fn [sum i]
            (+ sum (Math/abs (- (aget vec-1 i) (aget vec-2 i)))))
          0.0 (range (alength vec-1))))

;;;;---------------------------------------------------------------------------
;;;; Datatypes
;;;;---------------------------------------------------------------------------
(defrecord SOM
    [^"[[[D" nodes])

(defrecord Annotation
    [x y text])

(defrecord Coord
    [x y dist])

;;;;---------------------------------------------------------------------------
;;;; SOM functions
;;;;---------------------------------------------------------------------------
(defn make-som
  ([input-size x-size y-size]
   (make-som input-size x-size y-size
             (fn [] (double-array (repeatedly input-size rand)))))
  ([input-size x-size y-size input-vector-generator]
   "Create a Self-Organizing Map (with input size = length of start-wegihts)."
   (let [a (make-array Double/TYPE x-size y-size input-size)]
     (doseq [x (range x-size)]
       (doseq [y (range y-size)]
         (aset ^"[[D" (aget ^"[[[D" a (int x)) y (input-vector-generator))))
     (SOM. a))))

(defn- find-bmu [som target-vec dist-fn]
  "Coord of best matching unit."
   (let [nodes ^"[[[D" (:nodes som)]
     (reduce (fn [bmu x]
               (reduce (fn [bmu y]
                         (let [dist (dist-fn (aget ^"[[D" (aget nodes x) y)
                                             target-vec)]
                           (if (< dist (:dist bmu))
                             (assoc bmu :x x :y y :dist dist)
                             bmu)))
                       bmu (range (alength ^"[[D" (aget nodes x)))))
             (Coord. 0 0 (dist-fn (aget nodes (int 0) (int 0)) target-vec))
             (range (alength nodes)))))
          
(defn- improved-vec
  [^doubles target-vec ^doubles train-vec learning-rate dist-factor]
  "A new target-vec closer to train-vec."
  (let [v (aclone target-vec)]
    (doseq [i (range (alength ^"[D" target-vec))]
      (aset v i (+ (aget target-vec i) (* dist-factor
                                          learning-rate
                                          (- (aget train-vec i)
                                             (aget target-vec i))))))
    v))

(defn train [som sample-generator num-iterations dist-fn]
  "Train SOM."
  (let [new-nodes (aclone ^"[[[D" (:nodes som))
        x-size (alength new-nodes)
        y-size (alength ^"[[D" (aget new-nodes 0))
        map-radius (/ (max x-size y-size) 2)
        time-constant (/ num-iterations (Math/log map-radius))
        calc-radius (fn [iteration]
                      (* map-radius
                         (Math/exp (- (/ iteration time-constant)))))
        calc-learning-rate (fn [iteration]
                             (* 0.1 ; TODO: start-learn-rate good?
                                (Math/exp (- (/ iteration num-iterations)))))]
    (assoc
     som :nodes
     (reduce
      (fn [_ iteration]
        (let [radius (calc-radius iteration)
              sample-vec (sample-generator)
              bmu (find-bmu som sample-vec dist-fn)]
          ;; TODO: Don't go through everything - neighborhood should be enought
          (reduce
           (fn [_ r-idx]
             (reduce
              (fn [_ c-idx]
                (let [distance (Math/sqrt
                                (+ (Math/pow (- r-idx (:x bmu)) 2)
                                   (Math/pow (- c-idx (:y bmu)) 2)))]
                  (when (< distance radius)
                    (aset ^"[[D" (aget ^"[[D" new-nodes r-idx) c-idx
                          (improved-vec
                           (aget ^"[[D" (aget new-nodes r-idx) c-idx)
                           sample-vec
                           (calc-learning-rate iteration)
                           (Math/exp (- ; Distance factor
                                      (/
                                       (Math/pow distance 2)
                                       (* radius 2)))))))
                  new-nodes))
              nil (range y-size)))
           nil (range x-size))))
      nil (range num-iterations)))))

(defn u-matrix [som dist-fn]
  "Create Unified distance Matrix for SOM."
  (let [nodes ^"[[[D" (:nodes som)
        x-size (- (* (alength nodes) 2) 1)
        y-size (- (* (alength ^"[[D" (aget nodes 0)) 2) 1)
        umatrix (make-array Double/TYPE x-size y-size)]
    ;; First pass: Fill in distances between all nodes
    (doall
     (for [x (range x-size)
           y (range y-size)]
       (aset umatrix x y
             (cond (and (odd? x) (odd? y)) ; Both odd: Diagonal between nodes
                   (/ (+ (dist-fn (aget nodes (/ (dec x) 2) (/ (dec y) 2))
                                  (aget nodes (/ (inc x) 2) (/ (inc y) 2)))
                         (dist-fn (aget nodes (/ (dec x) 2) (/ (inc y) 2))
                                  (aget nodes (/ (inc x) 2) (/ (dec y) 2))))
                      2)
                   (odd? y) ; Only y odd: Between two nodes above/below
                   (dist-fn (aget nodes (/ x 2) (/ (dec y) 2))
                            (aget nodes (/ x 2) (/ (inc y) 2)))
                   (odd? x) ; Only x odd: Between two nodes left/right
                   (dist-fn (aget nodes (/ (dec x) 2) (/ y 2))
                            (aget nodes (/ (inc x) 2) (/ y 2)))
                   true ; Leave as it was
                   (aget umatrix x y)))))
    ;; Second pass: Fill in nodes using average of surrounding cells
    (doall
     (for [x (range 0 x-size 2)
           y (range 0 y-size 2)]
       (aset umatrix x y
             (cond (and (= x 0) (= y 0)) ; Lower left corner
                   (/ (+ (aget umatrix x (inc y))
                         (aget umatrix (inc x) (inc y))
                         (aget umatrix (inc x) y))
                      3)
                   (and (= x 0) (= y (dec y-size))) ; Upper left corner
                   (/ (+ (aget umatrix x (dec y))
                         (aget umatrix (inc x) (dec y))
                         (aget umatrix (inc x) y))
                      3)
                   (= x 0) ; Along left edge
                   (/ (+ (aget umatrix x (dec y))
                         (aget umatrix (inc x) (dec y))
                         (aget umatrix (inc x) y)
                         (aget umatrix (inc x) (inc y))
                         (aget umatrix  x (inc y)))
                      5)
                   (and (= x (dec x-size)) (= y 0)) ; Lower right corner
                   (/ (+ (aget umatrix (dec x) y)
                         (aget umatrix (dec x) (inc y))
                         (aget umatrix x (inc y)))
                      3)
                   (and (= x (dec x-size)) (= y (dec y-size))) ; Upper right
                   (/ (+ (aget umatrix x (dec y))
                         (aget umatrix (dec x) (dec y))
                         (aget umatrix (dec x) y))
                      3)
                   (= x (dec x-size)) ; Along right edge
                   (/ (+ (aget umatrix x (dec y))
                         (aget umatrix (dec x) (dec y))
                         (aget umatrix (dec x) y)
                         (aget umatrix (dec x) (inc y))
                         (aget umatrix x (inc y)))
                      5)
                   (= y 0) ; Along lower edge
                   (/ (+ (aget umatrix (dec x) y)
                         (aget umatrix (dec x) (inc y))
                         (aget umatrix x (inc y))
                         (aget umatrix (inc x) (inc y))
                         (aget umatrix (inc x) y))
                      5)
                   (= y (dec y-size)) ; Along upper edge
                   (/ (+ (aget umatrix (dec x) y)
                         (aget umatrix (dec x) (dec y))
                         (aget umatrix x (dec y))
                         (aget umatrix (inc x) (dec y))
                         (aget umatrix (inc x) y))
                      5)
                   true ; The rest: Average from everything around
                   (/ (+ (aget umatrix x (inc y))
                         (aget umatrix (inc x) (inc y))
                         (aget umatrix (inc x) y)
                         (aget umatrix (inc x) (dec y))
                         (aget umatrix x (dec y))
                         (aget umatrix (dec x) (dec y))
                         (aget umatrix (dec x) y)
                         (aget umatrix (dec x) (inc y)))
                      8)))))
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
      (let [biggest (reduce (fn [biggest y]
                             (max biggest
                                  (reduce
                                   (fn [biggest x]
                                     (max biggest (aget umatrix x y)))
                                   biggest
                                   (range  x-size))))
                            (aget umatrix 0 0)
                            (range y-size))]
        (doall (for [x (range x-size) y (range y-size)]
                 (aset umatrix x y (/ (aget umatrix x y) biggest))))
        umatrix))))

(defn pprint-u-matrix [^"[[D" u-matrix]
  (reduce (fn [_ y] ;; Note: Rotate -90 and flip to show x/y correctly
            (reduce (fn [_ x]
                      (print (format " %5f" (aget u-matrix x y))))
                    nil (range (alength u-matrix)))
            (print "\n"))
          nil (reverse (range (alength ^"[D" (aget u-matrix 0))))))

(defn export-u-matrix
  ([^"[[D" u-matrix png-filename]
   (export-u-matrix u-matrix png-filename nil))
  ([^"[[D" u-matrix png-filename annotations]
   "Export PNG file with grayscale weights and annotations coords."
   (let [scale 3
         x-size (alength u-matrix)
         width (* x-size scale)
         y-size (alength ^"[D" (aget u-matrix 0))
         height (* x-size scale)
         bi (java.awt.image.BufferedImage.
             width height java.awt.image.BufferedImage/TYPE_INT_ARGB)
         g (.createGraphics bi)]
     (.setRenderingHint g java.awt.RenderingHints/KEY_ANTIALIASING
                        java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
     (reduce (fn [_ x]
               (reduce (fn [_ y]
                         (.setColor g
                                    (new java.awt.Color
                                         (- 1.0 (float (aget u-matrix x y)))
                                         (- 1.0 (float (aget u-matrix x y)))
                                         (- 1.0 (float (aget u-matrix x y)))))
                         (.fillOval g
                                    (* x scale)
                                    (- height (* (inc y) scale)) scale scale))
                       nil (range y-size)))
             nil (range x-size))
     ;; Annotations
     (doseq [a annotations]
       (let [x-pos (int (* (:x a) 2 scale))
             y-pos (int (* (:y a) 2 scale))]
         (.setColor g (new java.awt.Color 0.0 0.0 0.0 0.5))
         (.setFont g (new java.awt.Font "Helvetica", 1, 17))
         (.drawOval g (inc x-pos) (inc y-pos) scale scale)
         (.drawString g ^String (:text a) (+ x-pos 3) (dec y-pos))
         (.setColor g (new java.awt.Color 1.0 0.0 0.0 0.7))
         (.setFont g (new java.awt.Font "Helvetica", 1, 16))
         (.drawOval g x-pos y-pos scale scale)
         (.drawString g ^String (:text a) (+ x-pos 2) (- y-pos 2))))
     (javax.imageio.ImageIO/write bi "png" (java.io.File. ^String png-filename)))))
