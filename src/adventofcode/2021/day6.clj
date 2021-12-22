(ns adventofcode.2021.day6
  (:require [clojure.test :refer [deftest testing is]]))


(def initial-state [3,4,3,1,2])

(def add* (fnil + 0))

(defn create-new-states-map [states-and-amount]
  (let [amount-of-new-babies (get states-and-amount 0)]
    (if (nil? amount-of-new-babies) {} {8 amount-of-new-babies, 6 amount-of-new-babies})))

(defn simulate-lanternfish [states days]
  (loop [states-and-amount (frequencies states), day 0]
    (if (< day days)
      (let [new-map (create-new-states-map states-and-amount)
            status-without-zero (into {} (filter #(-> % key (> 0)) states-and-amount))
            new-status (reduce-kv #(update %1 (dec %2) add* %3) new-map status-without-zero)]
        (recur new-status (inc day)))
      (apply + (for [[_ v ] states-and-amount] v)))))


(deftest test-simulate-lanternfish
  (testing "simulate 80 days "
    (is (= 5934 (simulate-lanternfish initial-state 80))))
  (testing "simulate 256 days "
    (is (= 26984457539 (simulate-lanternfish initial-state 256)))))
