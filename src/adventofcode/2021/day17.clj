(ns adventofcode.2021.day17
  (:require [clojure.test :refer :all]
            [clojure.tools.trace]
            [clojure.core.reducers :as r]))


(defn- parse-input [input]
  (let [[x1 x2 y1 y2] (->> input
                           (re-matches #"target area: x=(.*)\.\.(.*), y=(.*)\.\.(.*)")
                           (rest)
                           (map #(Integer/parseInt %)))]
    [[x1 x2] [y1 y2]]))


(defn- next-step [position velocity]
  (let [[px py] position,
        [vx, vy] velocity,
        new-px (+ px vx)
        new-py (+ py vy)
        new-vx (cond
                 (zero? vx) 0
                 (pos-int? vx) (dec vx)
                 (neg-int? vx) (inc vx))
        new-vy (dec vy)]
    [[new-px new-py] [new-vx new-vy]]))

(defn- inside-target-position? [position target-position]
  (let [[px py] position
        [[tx1 tx2] [ty1 ty2]] target-position]
    (and (>= px tx1) (<= px tx2) (>= py ty1) (<= py ty2))))

(defn- passed-target-position? [position target-position]
  (let [[px py] position
        [[tx1 tx2] [ty1 ty2]] target-position]
    (or (> px tx2) (< py ty1))))


(defn- highest-y [velocity target-position]
  (loop [position [0 0], velocity velocity, steps 0, highest-y 0]
    (let [[new-p new-v] (next-step position velocity)
          new-highest-y (if (> (second new-p) highest-y) (second new-p) highest-y)]
      (cond
        (inside-target-position? new-p target-position) highest-y
        (passed-target-position? new-p target-position) 0
        (> steps 10000) nil
        :else (recur new-p new-v (inc steps) new-highest-y)))))


(defn search-highest-y [target-position max-velocity-x max-velocity-y]
  (let [velocities (for [x (range max-velocity-x), y (range max-velocity-y)] [x y])]
    (loop [[v & r] velocities, high-y 0]
      (if (nil? v)
        high-y
        (let [max-y (highest-y v target-position)
              new-high-y (if (> max-y high-y) max-y high-y)]
          (recur r new-high-y))))))


(defn- valid-velocity? [initial-velocity target-position]
  (loop [position [0 0], velocity initial-velocity, steps 0]
    (let [[new-p new-v] (next-step position velocity)]
      (cond
        (inside-target-position? new-p target-position) true
        (passed-target-position? new-p target-position) false
        (> steps 10000) false
        :else (recur new-p new-v (inc steps))))))


(defn count-initial-velocity [target-position max-velocity-x max-velocity-y]
  (->> (for [x (range (- max-velocity-x) max-velocity-x)
             y (range (- max-velocity-y) max-velocity-y)]
         [x y])
       (r/filter #(valid-velocity? % target-position))
       r/foldcat
       count))


(deftest test-highest-y
  (testing "highest-y for [7 2]"
    (is (= 3 (highest-y [7 2] (parse-input "target area: x=20..30, y=-10..-5")))))
  (testing "highest-y for [9 0]"
    (is (= 0 (highest-y [9 0] (parse-input "target area: x=20..30, y=-10..-5")))))
  (testing "search"
    (is (= 45 (search-highest-y (parse-input "target area: x=20..30, y=-10..-5") 100 100))))
  )

(deftest count-velocities
  (testing "count-initial-velocity"
    (is (= 112 (count-initial-velocity (parse-input "target area: x=20..30, y=-10..-5") 100 100)))))
