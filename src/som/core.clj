(ns som.core)

(set! *warn-on-reflection* true)

(def start-learning-rate 1.0)

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

(defn means [seqs]
  (let [num-rows (count seqs)]
    (map (fn [col-sum]
           (/ col-sum num-rows))
         (reduce (fn [sums row]
                   (map (fn [col-sum col-val]
                          (+ col-sum col-val))
                        sums row))
                 seqs))))

(defn variances [seqs]
  (means
   (map (fn [row]
          (map (fn [col-val mean]
                 (Math/pow (- col-val mean) 2))
               row (means seqs)))
        seqs)))

(defn z-score-normalize [data]
  "Subtract coordinate mean from each coordinate value and divide by the standard deviation of that coordinate."
  (let [stdevs (map (fn [x] (Math/sqrt x)) (variances data))]
    (map (fn [row]
           (map (fn [col-val stdev mean] (/ (- col-val mean) stdev))
                row stdevs (means data)))
         data)))

(defn gaussian [distance sigma]
  (Math/exp (- (/ (* distance distance) (* sigma sigma 2)))))

;;;;---------------------------------------------------------------------------
;;;; Datatypes
;;;;---------------------------------------------------------------------------
(defrecord SOM ; A record that for semantic reasons wrap nodes
    [^"[[[D" nodes]) ; A 3D array of nodes with weights

(defrecord Annotation
    [x y text])

;;;;---------------------------------------------------------------------------
;;;; SOM functions
;;;;---------------------------------------------------------------------------
(defn make-som
  ([input-size x-size y-size]
   (make-som input-size x-size y-size
             (fn [] (double-array (repeatedly input-size rand)))))
  ([input-size x-size y-size input-vector-generator]
   "Make Self-Organizing Map (with input size = length of start-weights)."
   (let [a (make-array Double/TYPE x-size y-size input-size)]
     (doseq [x (range x-size)]
       (doseq [y (range y-size)]
         (aset ^"[[D" (aget ^"[[[D" a (int x)) y (input-vector-generator))))
     (SOM. a))))

(defn- ^"[[[D" clone-nodes [som]
  "Return a deep copy of a SOMs nodes."
  (let [old-nodes ^"[[[D" (:nodes som)
        new-som (make-som (alength ^"[D" (aget ^"[D" old-nodes 0 0))
                          (alength ^"[[[D" old-nodes)
                          (alength ^"[[D" (aget ^"[[D" old-nodes 0)))]
    (loop [x (dec (alength ^"[[[D" old-nodes))
           y (dec (alength ^"[[D" (aget ^"[[[D" old-nodes 0)))]
      (aset ^"[[[D" (:nodes new-som) x y (aclone ^"[D" (aget ^"[[[D" old-nodes x y)))
      (when (> x 0)
        (if (> y 0)
          (recur x (dec y))
          (recur (dec x) y))))
    (:nodes new-som)))

(defn- find-bmu [som target-vec dist-fn]
  "Coords {:x x :y y} of best matching unit."
  (let [nodes ^"[[[D" (:nodes som)]
    (dissoc
     (reduce (fn [bmu x]
               (reduce (fn [bmu y]
                         (let [dist (dist-fn (aget ^"[[D" (aget nodes x) y)
                                             target-vec)]
                           (if (< dist (:dist bmu))
                             (assoc bmu :x x :y y :dist dist)
                             bmu)))
                       bmu (range (alength ^"[[D" (aget nodes x)))))
             {:x 0 :y 0 :dist (dist-fn
                               (aget nodes (int 0) (int 0)) target-vec)}
             (range (alength nodes)))
     :dist)))

(defn train
  ([som sample-generator num-iterations]
   (train som sample-generator num-iterations {}))
  ([som sample-generator num-iterations options]
   "Train SOM.
options:
distance-fn: [^doubles vec-1 ^doubles vec-2] Calc distance between two vectors.
Defaults to euclidean-dist.
learning-rate-fn: [iteration tot-iterations] Calc learning rate for iteration.
neighborhood-fn: [distance radius] Calc distance factor.
Defaults to gaussian.
radius-fn: [iteration num-iterations] Calc radius around BMU for iteration.
Defaults to."
   (let [new-nodes (clone-nodes som)
         x-size (alength new-nodes)
         y-size (alength ^"[[D" (aget new-nodes 0))
         options
         {:distance-fn (get options :distance-fn euclidean-dist)
          :learning-rate-fn (get options :learning-rate-fn
                                 (fn [iteration total-iterations]
                                   (* start-learning-rate
                                      (Math/exp
                                       (- (/ iteration
                                             total-iterations))))))
          :neighborhood-fn (get options :neighborhood-fn gaussian)
          :radius-fn (get options :radius-fn
                          (fn [iteration total-iterations]
                            (* (/ (max x-size y-size) 2) ; Init radius
                               (Math/exp
                                (- (/ iteration total-iterations))))))}]
     (loop [iteration 0
            learning-rate ((:learning-rate-fn options) iteration num-iterations)
            radius ((:radius-fn options) iteration num-iterations)
            sample-vec ^doubles (sample-generator)
            bmu (find-bmu som sample-vec (:distance-fn options))]
       (if (< iteration (dec num-iterations))
         (do
           (reduce
            (fn [_ r-idx]
              (reduce
               (fn [_ c-idx]
                 (let [distance (Math/sqrt
                                 (+ (Math/pow (- r-idx (:x bmu)) 2)
                                    (Math/pow (- c-idx (:y bmu)) 2)))
                       distance-factor (gaussian distance radius)]
                   (when (and (< distance radius)
                              (> distance-factor 0.0))
                     (let [target-vec
                           ^doubles (aget ^"[[D" (aget new-nodes r-idx) c-idx)]
                       (doseq [i (range (alength ^"[D" target-vec))]
                         (aset target-vec i
                               (+ (aget target-vec i)
                                  (* distance-factor
                                     learning-rate
                                     (- (aget sample-vec i)
                                        (aget target-vec i))))))))
                   new-nodes))
               nil (range y-size)))
            nil (range x-size))
           (recur (inc iteration)
                  ((:learning-rate-fn options) (inc iteration) num-iterations)
                  ((:radius-fn options) (inc iteration) num-iterations)
                  (sample-generator)
                  (find-bmu som sample-vec (:distance-fn options))))
         (assoc som :nodes new-nodes))))))

(defn bmu-counts [som sample-generator iterations dist-fn]
  "Collect stats for BMU given iterations number of sample vectors."
  (reduce (fn [counts iteration]
            (let [bmu (find-bmu som (sample-generator) dist-fn)
                  cx (:x bmu)
                  cy (:y bmu)]
              (assoc counts [cx cy] (inc (get counts [cx cy] 0)))))
          {}
          (range iterations)))

(defn bmu-counts-w-class [som sample-w-class-generator iterations dist-fn]
  "Like bmu-counts but every sample is a map on the form {:class :vec}."
  (reduce (fn [counts iteration]
            (let [sample (sample-w-class-generator)
                  bmu (find-bmu som (:vec sample) dist-fn)
                  cx (:x bmu)
                  cy (:y bmu)
                  place [(:class sample) [cx cy]]]
              (assoc-in counts place (inc (get-in counts place 0)))))
          {}
          (range iterations)))
