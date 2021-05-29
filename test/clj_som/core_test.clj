(ns som.core-test
  (:require [clojure.test :refer :all]
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

;; (deftest test-u-matrix
;;   (testing "Testing if U-Matrix gets created properly."
;;     (let [a (to-array-2d [(make-array Double/TYPE 3)
;;                           (make-array Double/TYPE 3)
;;                           (make-array Double/TYPE 3)])]
;;       (aset a 0 0 (into-array Double/TYPE [1.0 2.0]))
;;       (aset a 0 1 (into-array Double/TYPE [1.0 2.0]))
;;       (aset a 0 2 (into-array Double/TYPE [1.0 1.0]))
;;       (aset a 1 0 (into-array Double/TYPE [1.0 1.0]))
;;       (aset a 1 1 (into-array Double/TYPE [1.0 1.0]))
;;       (aset a 1 2 (into-array Double/TYPE [1.0 1.0]))
;;       (aset a 2 0 (into-array Double/TYPE [1.0 3.0]))
;;       (aset a 2 1 (into-array Double/TYPE [1.0 1.0]))
;;       (aset a 2 2 (into-array Double/TYPE [1.0 0.0]))
;;       ;;
;;       (let [um (u-matrix {:nodes a} euclidean-dist)]
;;         (=
       
