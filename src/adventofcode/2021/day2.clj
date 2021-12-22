(ns adventofcode.2021.day2
  (:require [clojure.test :refer [deftest testing is]]))

(def forward :forward)
(def down :down)
(def up :up)

(def movements [forward 5 down 5 forward 8 up 3 down 8 forward 2])

(defn move-submarine [movements]
  (loop [x 0, depth 0, aim 0, movements movements]
    (let [direction (first movements), amount (second movements)]
      (cond
        (= direction forward) (recur (+ x amount) (+ depth (* amount aim)) aim (drop 2 movements))
        (= direction down)    (recur x depth (+ aim amount) (drop 2 movements))
        (= direction up)      (recur x depth (- aim amount) (drop 2 movements))
        :else (* x depth)))))

(deftest test-sonar-sweep
  (testing "basic movements"
    (is (= 900 (move-submarine movements)))))

