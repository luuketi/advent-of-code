(ns adventofcode.2021.day9
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.core.matrix :as mtx]))

(def input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n")

(defn heightmap [input]
  (let [splitted (->> input (str/split-lines) (map #(str/split % #"")))]
    (for [l splitted] (map #(Integer/parseInt %) l))))

(defn- height-value [map [r c]]
  (nth (nth map r) c))

(defn- neighbour-positions [the-map [r c]]
  (let [neighbours [[(dec r) c], [(inc r) c], [r (dec c)], [r (inc c)]]]
    (remove #(or (some neg-int? %)
                 (>= (second %) (mtx/column-count the-map))
                 (>= (first %) (mtx/row-count the-map))) neighbours)))

(defn- neighbours [the-map [r c]]
  (map #(height-value the-map %) (neighbour-positions the-map [r c])))

(defn lowest-points [the-map]
  (apply + (for [r (range (count the-map)),
                 c (range (count (first the-map)))
                 :let [value (height-value the-map [r c]),
                       neighbours (neighbours the-map [r c])]
                 :when (every? #(< value %) neighbours)]
             (inc value))))


(defn- delete-9 [the-map]
  (vec (for [row the-map]
         (vec (for [c row] (if (= 9 c) nil c))))))

(defn- find-unpainted-point [the-map]
  (let [rows (range (mtx/row-count the-map))
        columns (range (mtx/column-count the-map))
        posititons-and-values (for [r rows, c columns] [[r c] (height-value the-map [r c])])
        positives-values (filter #(nat-int? (second %)) posititons-and-values)]
    (if (empty? positives-values) [] (first (first positives-values)))))

(defn- paint-zone [the-map [r c] color]
  (let [painted-map (mtx/set-selection the-map r c color)
        neighbour-positions (neighbour-positions painted-map [r c])
        posititons-and-values (for [[r c] neighbour-positions] [[r c] (height-value the-map [r c])])
        valid-neighbours (filter #(nat-int? (second %)) posititons-and-values)
        new-painted-map (loop [[[[r c] _] & x] valid-neighbours, painted-map painted-map]
                          (let [painted (mtx/set-selection painted-map r c color)]
                            (if (nil? x) painted (recur x painted))))]
    (if (empty? valid-neighbours)
      new-painted-map
      (loop [[[neighbour _] & x] valid-neighbours, painted-map new-painted-map]
        (let [painted-map (paint-zone painted-map neighbour color)]
          (if (nil? x) painted-map (recur x painted-map)))))))

(defn- paint-map [the-map]
  (loop [painted-map (delete-9 the-map), color -1]
    (let [pos-to-paint (find-unpainted-point painted-map)]
      (if (empty? pos-to-paint)
        painted-map
        (recur (paint-zone painted-map pos-to-paint color) (dec color))))))

(defn three-largest-basins [the-map]
  (->> the-map
       (paint-map)
       (flatten)
       (remove nil?)
       (frequencies)
       (sort-by second)
       (take-last 3)
       (map second)
       (apply *)
       ))

(three-largest-basins (heightmap input))

(deftest test-day-9
  (testing "lowest points "
    (is (= 15 (lowest-points (heightmap input)))))
  (testing "lowest points "
    (is (= 1134 (three-largest-basins (heightmap input)))))
  )

