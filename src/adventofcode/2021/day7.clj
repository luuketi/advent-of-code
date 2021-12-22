(ns adventofcode.2021.day7
  (:require [clojure.test :refer [deftest testing is]]))


(def positions [16, 1, 2, 0, 4, 2, 7, 1, 2, 14])

(defn calculate-fuel [positions]
  (let [max-pos (apply max positions)]
    (apply min (for [i (range max-pos)] (apply + (for [pos positions] (Math/abs (- i pos))))))))

(defn calculate-incremental-fuel [positions]
  (let [max-pos (apply max positions)
        fuel (for [i (range max-pos)]
               (apply + (for [pos positions]
                          (apply + (range 1 (inc (Math/abs (- i pos))))))))]
    (apply min fuel)))


(deftest test-calculate-fuel
  (testing "calculate-fuel "
    (is (= 37 (calculate-fuel positions))))
  (testing "calculate-fuel "
    (is (= 168 (calculate-incremental-fuel positions)))))
