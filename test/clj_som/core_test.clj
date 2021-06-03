(ns som.core-test
  (:require [clj-async-profiler.core :as prof]
            [clojure.test :refer :all]
            [criterium.core :as c]
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
      (is (= [1 0]
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
         (slurp "test/clj_som/iris-ammended.csv")
         #"\n"))))

(deftest test-train-iris
  (testing "Testing training and export with Iris dataset."
    (let [iris (read-iris-data)
          sample (fn []
                   (into-array Double/TYPE
                               (take 4 (nth iris (rand (count iris))))))
          input-vec-gen (fn [] (double-array [(rand 5) (rand 5) (rand 5)]))
          som (train (make-som 4 64 64 input-vec-gen) sample 500 euclidean-dist)
          um (u-matrix som euclidean-dist)
          ]
      ;;(pprint-u-matrix um)
      (export-u-matrix um "iris.png" (list (->Annotation 32 32 "Hello")))
      )))
