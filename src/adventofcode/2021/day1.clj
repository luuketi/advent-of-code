(ns adventofcode.2021.day1
  (:require [clojure.test :refer [deftest testing is]]))


(def report [199 200 208 210 200 207 240 269 260 263])

(defn sonar-sweep [report]
  (->>  (map #(< % %2) report (subvec report 1))
        (filter identity)
        (count)))

(defn sonar-sweep-window [report]
  (->> (map + report (subvec report 1) (subvec report 2))
       (vec)
       (sonar-sweep)))



(deftest test-sonar-sweep
  (testing "basic report"
    (is (= 7 (sonar-sweep report))))
  (testing "windowed basic report"
    (is (= 5 (sonar-sweep-window report)))))

