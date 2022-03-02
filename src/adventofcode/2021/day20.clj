(ns adventofcode.2021.day20
  (:require [clojure.test :refer :all]
            [clojure.tools.trace]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.core.reducers :as r]
            [clojure.core.matrix :as mtx]))


(def enhacement-algorithm-string "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\n#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\n.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\n.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\n.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\n...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\n..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#")

(def input-image "#..#.\n#....\n##..#\n..#..\n..###")

(defn- enhacement-algorithm [enhacement-algorithm-string]
  (str/replace enhacement-algorithm-string #"\n" ""))

(defn- make-image [input-image]
  (->> input-image
       (str/split-lines)
       (mapv #(str/split % #""))))


(defn- make-board [image apply-steps]
  (let [image-size (mtx/column-count image)
        board-size (+ (* 2 apply-steps) image-size)
        board (for [_ (range board-size)] (for [_ (range board-size)] "."))
        image-positions (for [x (range image-size), y (range image-size)] [x y])]
    (loop [[[x y] & r] image-positions, board board]
      (if (nil? x)
        board
        (recur r (mtx/set-selection board (+ apply-steps x) (+ apply-steps y) (mtx/select image x y)))))))


(defn- pixels->number [pixels]
  (let [binary (->> pixels (map #(if (= \# %) 1 0)) (apply str))]
    (Integer/parseInt binary 2)))


(defn- output-pixel [pixel enhacement-algorithm]
  (->> pixel pixels->number (nth enhacement-algorithm) (str)))


(defn- neighbour-positions [x y]
  [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
   [x (dec y)] [x y] [x (inc y)]
   [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])

(defn- border-or-outside-board? [board x y]
  (or (some neg-int? [x y])
      (some zero? [x y])
      (>= y (dec (mtx/column-count board)))
      (>= x (dec (mtx/row-count board)))))

(defn- neighbours [board [x y]]
  (->> (neighbour-positions x y)
       (map (fn [[x y]] (if (border-or-outside-board? board x y) (mtx/select board 0 0) (mtx/select board x y)) ))
       (apply str)))


(defn- output-board [board enhacement-algorithm]
  (let [size (mtx/column-count board)
        positions (for [x (range size), y (range size)] [x y])
        original-board (for [x (range size)] (for [y (range size)] (mtx/select board x y)))]
    (loop [[[x y] & r] positions, board board]
      (if (nil? x)
        board
        (let [neighbours (neighbours original-board [x y])
              new-pixel (output-pixel neighbours enhacement-algorithm)]
          (recur r (mtx/set-selection board x y new-pixel)))))))


(defn apply-algorithm [input-image enhacement-algorithm-string apply-times]
  (let [image (make-image input-image)
        board (make-board image apply-times)
        enhacement-algorithm (enhacement-algorithm enhacement-algorithm-string)]
    (loop [board board, times 0]
      (println "step:" times)
      (if (= times apply-times)
        board
        (recur (output-board board enhacement-algorithm) (inc times))))))

(defn count-lits [board]
  (->> board flatten (filter #(= % "#")) count))


(deftest test-count
  (testing "simple"
    (is (= 35 (count-lits (apply-algorithm input-image enhacement-algorithm-string 2))))))

