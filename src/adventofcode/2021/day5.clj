(ns adventofcode.2021.day5
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))


(def input "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n")

(def coordinates
  (vec (for [line (str/split-lines input)]
         (vec (for [coordinate (str/split line #" -> ")]
                (vec (for [v (str/split coordinate #",")] (Integer/parseInt v))))))))

(defn is-vert-or-horiz? [coord]
  (or (= (first (first coord)) (first (second coord)))
      (= (second (first coord)) (second (second coord)))))

(defn make-board [size]
  (vec (for [x (range size)] (vec (for [y (range size)] 0)))))

(defn expand-coordinate [[[x1 y1] [x2 y2]]]
  (let [[r1 r2]
      (cond
        (and (> x1 x2) (> y1 y2)) [(range x2 (inc x1)) (range y2 (inc y1))]
        (and (> x1 x2) (< y1 y2)) [(range x1 (dec x2) -1) (range y1 (inc y2))]
        (and (< x1 x2) (> y1 y2)) [(range x1 (inc x2)) (range y1 (dec y2) -1)]
        (and (< x1 x2) (< y1 y2)) [(range x1 (inc x2)) (range y1 (inc y2))]
        (= x1 x2) (let [y (range (min y1 y2) (inc (max y1 y2)))
                        x (repeat (count y) x1)] [x y])
        (= y1 y2) (let [x (range (min x1 x2) (inc (max x1 x2)))
                                y (repeat (count x) y1)] [x y]))]
    (vec (map vector r1 r2))))

(defn mark-board [board [x y]]
  (vec (for [i (range (count (first board)))
        :let [line (nth board i)]]
    (if (= i x)
      (update-in line [y] inc)
      line))))

(defn two-overlaps-points [coordinates]
  (let [empty-board (make-board 1000),
        filtered-coordinates ( apply concat (map expand-coordinate (filter is-vert-or-horiz? coordinates)))
        board (loop [[coord & r] filtered-coordinates, board empty-board]
                (if (nil? r)
                  (mark-board board coord)
                  (recur r (mark-board board coord))))]
    (apply + (for [line board] (count (filter #(> % 1) line))))))

(defn two-overlaps-points-with-diagonals [coordinates]
  (let [empty-board (make-board 1000),
        board (loop [[coord & r] (apply concat (map expand-coordinate coordinates)), board empty-board]
                (if (nil? r)
                  (mark-board board coord)
                  (recur r (mark-board board coord))))]
    (apply + (for [line board] (count (filter #(> % 1) line))))))

(deftest test-sonar-sweep
  (testing "check horizontal & vertical points "
    (is (= [[2 5] [3 5] [4 5] [5 5] [6 5]] (expand-coordinate [[6 5] [2 5]]))))
  (testing "check horizontal & vertical points "
    (is (= [[2 2] [3 3] [4 4]] (expand-coordinate [[2 2] [4 4]]))))
  (testing "check horizontal & vertical points "
    (is (= [[0 0] [1 1] [2 2]] (expand-coordinate [[2 2] [0 0]]))))
  (testing "check horizontal & vertical points "
    (is (= [[2 2] [1 3] [0 4]] (expand-coordinate [[2 2] [0 4]]))))
  (testing "check horizontal & vertical points "
    (is (= [[2 2] [3 1] [4 0]] (expand-coordinate [[2 2] [4 0]]))))
  (testing "check horizontal & vertical points "
    (is (= [[2 2] [3 3] [4 4]] (expand-coordinate [[2 2] [4 4]]))))
  (testing "check horizontal & vertical points "
    (is (= [[2 4] [3 4] [4 4]] (expand-coordinate  [[2 4] [4 4]]))))
  (testing "check horizontal & vertical points "
    (is (= [[4 2] [4 3] [4 4]] (expand-coordinate [[4 2] [4 4]]))))
  (testing "check horizontal & vertical points "
    (is (= 5 (two-overlaps-points coordinates))))
  (testing "check all points "
    (is (= 12 (two-overlaps-points-with-diagonals coordinates)))))
