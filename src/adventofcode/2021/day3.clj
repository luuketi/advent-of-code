(ns adventofcode.2021.day3
  (:require [clojure.test :refer [deftest testing is]]))

(def diagnostic-report ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])


(defn mode [order data]
  (let [frequencies (->> data (sort) (frequencies) (sort-by second order))
        are-equals? (apply = (for [[k v] frequencies] v))]
    (if are-equals?
      (if (= order >) 0 1)
      (first (last frequencies)))))


(def most-common (partial mode >))
(def least-common (partial mode <))

(defn b->i [b]
  (Integer/parseInt b 2))

(defn bits-by-mode [mode-func report]
  (let [size (count (first report))]
    (apply str (for [i (range size)] (mode-func (map #(nth % i) report))))))

(defn rating-calculator [mode-func report]
  (loop [i 0, report report]
    (let [bits (bits-by-mode mode-func report )]
      (if (= 1 (count report))
        (first report)
        (recur (inc i) (filter #(= (nth bits i) (nth % i)) report))))))

(defn power-consumption [report]
  (let [gama-rate (bits-by-mode most-common report)
        epsilon-rate (bits-by-mode least-common report)]
    (* (b->i gama-rate) (b->i epsilon-rate))))

(defn life-support-rating [report]
  (let [oxygen-generator-rating (rating-calculator most-common report)
        CO2-scrubber-rating (rating-calculator least-common report)]
    (* (b->i oxygen-generator-rating) (b->i CO2-scrubber-rating))))


(deftest test-sonar-sweep
  (testing "basic power-consumption diagnostic"
    (is (= 198 (power-consumption diagnostic-report))))
  (testing "basic life-support-rating diagnostic"
    (is (= 230 (life-support-rating diagnostic-report)))))
