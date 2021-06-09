(ns som.core-test
  (:require [clj-async-profiler.core :as prof]
            [clojure.pprint :as pprint]
            [clojure.test :refer :all]
            [criterium.core :as c]
            [som.u-matrix :refer :all]
            [som.core :refer :all]))

(deftest test-euclidean-dist
  (testing "Testing euclidean distance function."
    (is (= (Math/sqrt 2)
           (euclidean-dist (into-array Double/TYPE [1.0 0.0 0.0])
                           (into-array Double/TYPE [0.0 1.0 0.0]))))))

(deftest test-manhattan-dist
  (testing "Testing euclidean distance function."
    (is (= 2.0
           (manhattan-dist (into-array Double/TYPE [1.0 0.0 0.0])
                           (into-array Double/TYPE [0.0 1.0 0.0]))))))

(deftest test-find-bmu
  (testing "Testing find-bmu function."
    (let [a (to-array-2d [(make-array Double/TYPE 2)
                          (make-array Double/TYPE 2)])]
      (aset a 0 0 (into-array Double/TYPE [0.5 0.1]))
      (aset a 0 1 (into-array Double/TYPE [0.6 0.1]))
      (aset a 1 0 (into-array Double/TYPE [0.5 0.2]))
      (aset a 1 1 (into-array Double/TYPE [0.4 0.2]))
      (is (= {:x 1 :y 0}
             (#'som.core/find-bmu {:nodes a}
                                  (into-array Double/TYPE [0.49 0.21])
                                  euclidean-dist))))))

(deftest test-u-matrix
  (testing "Testing if U-Matrix gets created properly."
    (let [a (to-array-2d [(make-array Double/TYPE 3)
                          (make-array Double/TYPE 3)
                          (make-array Double/TYPE 3)])]
      (aset a 0 0 (into-array Double/TYPE [1.0 2.0]))
      (aset a 0 1 (into-array Double/TYPE [1.0 2.0]))
      (aset a 0 2 (into-array Double/TYPE [1.0 1.0]))
      (aset a 1 0 (into-array Double/TYPE [1.0 1.0]))
      (aset a 1 1 (into-array Double/TYPE [1.0 1.0]))
      (aset a 1 2 (into-array Double/TYPE [1.0 1.0]))
      (aset a 2 0 (into-array Double/TYPE [1.0 3.0]))
      (aset a 2 1 (into-array Double/TYPE [1.0 1.0]))
      (aset a 2 2 (into-array Double/TYPE [1.0 2.0]))
      ;;
      (let [um (u-matrix {:nodes a} euclidean-dist)]
        (pprint-u-matrix um)
        (export-u-matrix um "u-matrix.png")))))
       
(defn read-iris-data []
  (map (fn [line]
         (map (fn [col]
                (try (Double/parseDouble col)
                     (catch Exception e col)))
              (clojure.string/split line #",")))
       (rest ; Skip header
        (clojure.string/split
         (slurp "test/som/iris-ammended.csv")
         #"\n"))))

(deftest test-train-iris
  (testing "Testing training and export with Iris dataset."
    (let [iris (read-iris-data)
          normalized-iris (z-score-normalize iris)
          sample (fn []
                   (into-array Double/TYPE
                               (take 4 (nth iris (rand (count iris))))))
          norm-sample (fn []
                        (into-array Double/TYPE
                                    (take 4 (nth normalized-iris (rand (count iris))))))
          sample-w-class (fn []
                           (let [s (nth iris (rand (count iris)))]
                             {:vec (into-array Double/TYPE
                                               (take 4 s))
                              :class (clojure.string/replace
                                      (nth s 4) #"Iris-" "")}))
          norm-sample-w-class (fn []
                                (let [idx (rand (count normalized-iris))
                                      c (nth iris idx)
                                      s (nth normalized-iris idx)]
                                  {:vec (into-array Double/TYPE
                                                    (take 4 s))
                                   :class (clojure.string/replace
                                      (nth c 4) #"Iris-" "")}))
          input-vec-gen (fn [] (double-array
                                [;(+ 4.2 (rand 3.8))
                                 ;(+ 1.9 (rand 2.6))
                                 ;(+ 1.0 (rand 6.0))
                                        ;(rand 2.6)
                                 (rand 2)
                                 (rand 2)
                                 (rand 2)
                                 (rand 2)]))
          ;; Dim roughly = 5 * sqrt num-samples
          som (train (make-som 4 60 60 input-vec-gen) norm-sample 10)
          um (u-matrix som euclidean-dist)
          ;;centers (take-last 4 (sort-by val (:bmu-counts som)))
          freqs (bmu-counts-w-class
                 som
                 norm-sample-w-class
                 150
                 euclidean-dist)
          ]
      ;;(pprint/pprint freqs)
      ;;(println normalized-iris)
      ;;(pprint-u-matrix um)
      (export-u-matrix um "iris.png"
                       (map (fn [f]
                              (let [class (key f)
                                    [pos cnt] (last (sort-by val (val f)))
                                    text (str class "(" cnt ")")]
                                (->Annotation (first pos) (second pos) text)))
                            freqs)))))


(def color-dataset
  (->
   (reduce
    (fn [data _]
      (cons
       (let [r (rand 8)]
         (cond
           ;; Red-ish
           (> r 7)
           [(+ 0.98 (rand 0.02)) (rand 0.04) (rand 0.04)]
           ;; Blue-ish
           (> r 5)
           [(rand 0.04) (rand 0.04) (+ 0.51 (rand 0.02))]
           ;; Orange-ish
           (> r 6)
           [(+ 0.98 (rand 0.02)) (+ 0.39 (rand 0.02)) (+ 0.24 (rand 0.02))]
           ;; Light blue-ish
           (> r 4)
           [(rand 0.04) (rand 0.04) (+ 0.98 (rand 0.02))]
           ;; Green-ish
           (> r 3)
           [(rand 0.01) (+ 0.5 (rand 0.2)) (+ 0.25 (rand 1))]
           ;; Light green-ish
           (> r 2)
           [(rand 0.04) (+ 0.98 (rand 0.02)) (rand 0.04)]
           ;; Yellow-ish
           (> r 1)
           [(+ 0.988 (rand 0.012)) (+ 0.98 (rand 0.02)) (+ 0.19 (rand 0.02))]
           ;; Purple-ish
           (> r 0)
           [(+ 0.988 (rand 0.012)) (rand 0.01) (+ 0.988 (rand 0.012))]))
       data))
    nil
    (range 100))
   identity))

;(defn normalize-vectors [nodes]

(defn display-colors [som]
  (let [nodes (:nodes som)
        width (alength nodes)
        scale 5
        height (alength (aget nodes 0))
        bi (java.awt.image.BufferedImage.
            (* width scale) (* height scale)
             java.awt.image.BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)
        clamp (fn [v]  (cond (< v 0) (do 0.0)
                             (> v 1) 1.0
                             true v))]
    (.setRenderingHint g java.awt.RenderingHints/KEY_ANTIALIASING
                       java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
    ;; Draw vectors as colors
    (reduce (fn [_ x]
              (reduce (fn [_ y]
                        (let [w (aget nodes x y)]
                          (.setColor g
                                     (new java.awt.Color
                                          (double (clamp (aget w 0)))
                                          (double (clamp (aget w 1)))
                                          (double (clamp (aget w 2)))))
                          (.fillRect g (* x scale)
                                     (- (* height scale) (* (inc y) scale))
                                     scale scale)))
                      nil (range height)))
            nil (range width))
    (javax.imageio.ImageIO/write bi "png" (java.io.File. "colors.png"))))

(deftest test-train-colors
  (let [color-sample (fn []
                       (into-array Double/TYPE
                                   (nth color-dataset
                                        (rand (count color-dataset)))))
        input-vec-gen (fn [] (double-array
                              [(rand)(rand)(rand)]))
        ;; Dim roughly = 5 * sqrt num-samples
        som (train (make-som 3 63 63 input-vec-gen) color-sample 10)
        um (u-matrix som euclidean-dist)
        ]
    (display-colors som)
    (export-u-matrix um "colors-um.png")))
    
