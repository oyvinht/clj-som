(ns som.u-matrix)

(set! *warn-on-reflection* true)

(defn u-matrix [som dist-fn]
  "Create Unified distance Matrix for SOM."
  (let [units ^"[[[D" (:nodes som)
        x-size (- (* (alength units) 2) 1)
        y-size (- (* (alength ^"[[D" (aget units 0)) 2) 1)
        umatrix (make-array Double/TYPE x-size y-size)]
    ;; First pass: Fill in distances between all units
    (doall
     (for [x (range x-size)
           y (range y-size)]
       (aset umatrix x y
             (cond (and (odd? x) (odd? y)) ; Both odd: Diagonal between units
                   (/ (+ (dist-fn (aget units (/ (dec x) 2) (/ (dec y) 2))
                                  (aget units (/ (inc x) 2) (/ (inc y) 2)))
                         (dist-fn (aget units (/ (dec x) 2) (/ (inc y) 2))
                                  (aget units (/ (inc x) 2) (/ (dec y) 2))))
                      2)
                   (odd? y) ; Only y odd: Between two units above/below
                   (dist-fn (aget units (/ x 2) (/ (dec y) 2))
                            (aget units (/ x 2) (/ (inc y) 2)))
                   (odd? x) ; Only x odd: Between two units left/right
                   (dist-fn (aget units (/ (dec x) 2) (/ y 2))
                            (aget units (/ (inc x) 2) (/ y 2)))
                   true ; Leave as it was
                   (aget umatrix x y)))))
    ;; Second pass: Fill in units using average of surrounding cells
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
             y-pos (- height (int (* (:y a) 2 scale)))]
         (.setColor g (new java.awt.Color 0.0 0.0 0.0 0.5))
         (.setFont g (new java.awt.Font "Helvetica", 1, 16))
         (.drawOval g (inc x-pos) (inc y-pos) scale scale)
         (.drawString g ^String (:text a) (+ x-pos 3) (- y-pos 1))
         (.setColor g (new java.awt.Color 1.0 0.0 0.0 0.7))
         (.setFont g (new java.awt.Font "Helvetica", 1, 16))
         (.drawOval g x-pos y-pos scale scale)
         (.drawString g ^String (:text a) (+ x-pos 2) (- y-pos 2))))
     (javax.imageio.ImageIO/write
      bi "png" (java.io.File. ^String png-filename)))))
