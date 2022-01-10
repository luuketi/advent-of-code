(ns adventofcode.2021.day13
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))


(def input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5\n")

(defn parse-input [input]
  (letfn [(parse-int [c] (Integer/parseInt c))]
    (let [[points folds] (str/split input #"\n\n")
          points (->> points (str/split-lines) (map #(str/split % #",")) (map #(map parse-int %)))
          folds (->> folds (str/split-lines) (map #(str/split % #" ")) (map last) (map #(str/split % #"=")))]
      [points folds])))

(defn- matrix [points]
  (let [max-x (inc (apply max (map first points)))
        max-y (inc (apply max (map second points)))
        empty-matrix (into {} (for [x (range max-x), y (range max-y)] [[x y] 0]))]
    (loop [[[x y] & r] points, m empty-matrix]
      (if (nil? x) {:matrix m, :max-x max-x, :max-y max-y} (recur r (update m [x y] (constantly 1)))))))

(defn- fold-y [matrix y-to-fold]
  (let [max-x (:max-x matrix)
        max-y (:max-y matrix)
        matrix (:matrix matrix)
        new-matrix (into {} (for [x (range max-x), y (range y-to-fold)] [[x y] (get matrix [x y])]))
        points-to-fold (apply concat (for [y (range (inc y-to-fold) max-y)]
                                       (for [x (range max-x)] [x y])))]
    (loop [[[x y] & r] points-to-fold, m new-matrix]
      (if (nil? x)
        {:matrix m, :max-x max-x, :max-y y-to-fold}
        (let [y-to-be-folded (- (* 2 y-to-fold) y)
              val-to-be-replaced (get matrix [x y-to-be-folded])
              new-val (if (= 1 val-to-be-replaced) 1 (get matrix [x y]))]
          (recur r (update m [x y-to-be-folded] (constantly new-val))))))))

(defn- fold-x [matrix x-to-fold]
  (let [max-x (:max-x matrix)
        max-y (:max-y matrix)
        matrix (:matrix matrix)
        new-matrix (into {} (for [x (range x-to-fold), y (range max-y)] [[x y] (get matrix [x y])]))
        points-to-fold (apply concat (for [y (range max-y)] (for [x (range (inc x-to-fold) max-x)] [x y])))]
    (loop [[[x y] & r] points-to-fold, m new-matrix]
      (if (nil? x)
        {:matrix m, :max-x x-to-fold, :max-y max-y}
        (let [x-to-be-folded (- (* 2 x-to-fold) x)
              val-to-be-replaced (get matrix [x-to-be-folded y])
              new-val (if (= 1 val-to-be-replaced) 1 (get matrix [x y]))]
          (recur r (update m [x-to-be-folded y] (constantly new-val))))))))

(defn print-matrix [matrix]
  (let [m (:matrix matrix)]
    (pprint/pprint (for [y (range (:max-y matrix))] (for [x (range (:max-x matrix))] (get m [x y]))))))

(defn fold-matrix [input]
  (let [[points folds] (parse-input input)]
    (loop [[[coord val] & r] folds, m (matrix points)]
      (if (nil? coord)
        m
        (recur r (if (= coord "x") (fold-x m (Integer/parseInt val)) (fold-y m (Integer/parseInt val))))))))

(defn count-dots [input]
  (apply + (map second (:matrix (fold-matrix input)))))


(deftest counting-dots
  (testing "counting dots - fold y"
    (is (= 17 (count-dots "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\n")))))
(testing "counting dots - fold x"
  (is (= 16 (count-dots "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5\n"))))
