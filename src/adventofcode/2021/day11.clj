(ns adventofcode.2021.day11
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.core.matrix :as mtx]))

(def input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")

(defn- board [input]
  (let [splitted (->> input (str/split-lines) (map #(str/split % #"")))]
    (for [l splitted] (map #(Integer/parseInt %) l))))

(defn- level [map [r c]]
  (nth (nth map r) c))

(defn- neighbour-positions [levels [r c]]
  (let [neighbours [[(dec r) (dec c)] [(dec r) c] [(dec r) (inc c)]
                    [r (dec c)] [r (inc c)]
                    [(inc r) (dec c)] [(inc r) c] [(inc r) (inc c)]]]
    (remove #(or (some neg-int? %)
                 (>= (second %) (mtx/column-count levels))
                 (>= (first %) (mtx/row-count levels))) neighbours)))

(defn- increment-neighbours [levels [r c]]
  (letfn [(increment-position [levels [r c]] (mtx/set-selection levels r c (inc (level levels [r c]))))]
    (loop [[p & r] (neighbour-positions levels [r c]), levels levels]
      (if (nil? r) (increment-position levels p) (recur r (increment-position levels p))))))

(defn- increment-all-neighbours [levels positions]
  (loop [[positions & r] positions, levels levels]
    (let [new-levels (increment-neighbours levels positions)]
      (if (nil? r) new-levels (recur r new-levels)))))

(defn- positions-to-flash [levels]
  (for [r (range (mtx/row-count levels))
        c (range (mtx/column-count levels))
        :when (> (level levels [r c]) 9)]
    [r c]))

(defn- increment-loop [levels]
  (letfn [(flash-positions [levels] (for [l levels] (flatten (for [val l] (if (or (> val 9) (< val 0)) 0 val)))))
          (delete-already-flashed [levels] (for [l levels] (flatten (for [val l] (if (> val 9) -200 val)))))]
    (loop [levels levels]
      (let [positions-to-flash (positions-to-flash levels)]
        (if (empty? positions-to-flash)
          (flash-positions levels)
          (recur (increment-all-neighbours (delete-already-flashed levels) positions-to-flash)))))))

(defn- change [levels]
  (letfn [(number-of-flashes [levels] (apply + (for [l levels] (count (filter #(= % 0) l)))))
          (increment-levels [levels] (for [l levels] (map inc l)))]
    (let [incremented-levels (increment-levels levels)]
      (if (empty? (positions-to-flash incremented-levels))
        [(number-of-flashes incremented-levels) incremented-levels]
        (let [new-levels (increment-loop incremented-levels)
              number-of-flashes (number-of-flashes new-levels)]
          [number-of-flashes new-levels])))))

(defn play [input steps]
  (loop [[_ & r] (range steps), board (board input), flashes-counter 0]
    (let [[flashes board] (change board)
          new-flashes (+ flashes flashes-counter)]
      (if (nil? r) new-flashes (recur r board new-flashes)))))

(defn all-flashing-step [input]
  (let [levels (board input)
        octopuses-quantity (apply * (mtx/shape levels))]
    (loop [step 1, board levels]
      (let [[flashes board] (change board)]
        (if (= flashes octopuses-quantity) step (recur (inc step) board))))))


(deftest flashes
  (testing "1 step"
    (is (= [9 '((3 4 5 4 3) (4 0 0 0 4) (5 0 0 0 5) (4 0 0 0 4) (3 4 5 4 3))]
           (change (board "11111\n19991\n19191\n19991\n11111")))))
  (testing "2 steps"
    (is (= [0 '((4 5 6 5 4) (5 1 1 1 5) (6 1 1 1 6) (5 1 1 1 5) (4 5 6 5 4))]
           (change (second (change (board "11111\n19991\n19191\n19991\n11111")))))))
  (testing "100 steps"
    (is (= 1656
           (play "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526" 100)))))

(deftest all-flashing-together
  (testing "which step is all flashing"
    (is (= 195 (all-flashing-step input)))))
