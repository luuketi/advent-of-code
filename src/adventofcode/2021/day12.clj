(ns adventofcode.2021.day12
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))


(defn parse-input [input]
  (let [relationships (->> input (str/split-lines) (map #(str/split % #"-")))
        all-relationships (apply concat (for [[a b] relationships] [[a b] [b a]]))
        unique-nodes (into #{} (map first all-relationships))]
    (into {} (for [node unique-nodes] [node (mapv second (filter #(= node (first %)) all-relationships))]))))

(defn- neighbours [relationships node]
  (get relationships node))

(defn- complete-path? [path]
  (= "end" (last path)))

(def small-cave?
  (memoize
    (fn [cave]
      (every? #(Character/isLowerCase %) cave))))

(defn- valid-path? [path]
  (let [last-node-is-end (= "end" (last path))
        freqs (frequencies (filter small-cave? path))
        small-cave-duplicated (not (some #(> (second %) 1) freqs))]
    (or last-node-is-end small-cave-duplicated)))

(defn- new-paths-from-neighbours [relationships path]
  (for [n (neighbours relationships (last path))] (into path [n])))

(defn- make-paths [relationships valid-path-f starting-paths]
  (loop [[path & r] starting-paths, complete-paths []]
    (if (empty? path)
      complete-paths
      (let [new-paths (new-paths-from-neighbours relationships path)
            valid-paths (filter valid-path-f new-paths)
            new-complete-paths (into complete-paths (filter complete-path? valid-paths))
            new-incomplete-paths (into r (remove #(some #{%} new-complete-paths) valid-paths))]
        (recur new-incomplete-paths new-complete-paths)))))

(defn count-paths [input valid-path-f]
  (count (make-paths (parse-input input) valid-path-f [["start"]])))

(defn- valid-path-v2? [path]
  (if (= "end" (last path))
    true
    (let [small-caves (filter small-cave? path)
          freqs (frequencies small-caves)
          more-than-twice-caves (some #(> (second %) 2) freqs)
          more-than-one-start (> (get freqs "start") 1)]
      (if (or more-than-twice-caves more-than-one-start)
        false
        (let [freqs-of-freqs (frequencies (map second freqs))
              twice (get freqs-of-freqs 2)
              valid-twice-cave (or (= nil twice) (= 1 twice))]
          valid-twice-cave)))))

(defn- parallel-count-paths [input valid-path-f]
  (let [yq (future (count (make-paths (parse-input input) valid-path-f [["start" "yq"]])))
        ya-yq (future (count (make-paths (parse-input input) valid-path-f [["start" "YA" "yq"]])))
        ya-nf (future (count (make-paths (parse-input input) valid-path-f [["start" "YA" "nf"]])))
        ya-yi (future (count (make-paths (parse-input input) valid-path-f [["start" "YA" "yi"]])))
        nf (future (count (make-paths (parse-input input) valid-path-f [["start" "nf"]])))]
    (+ @yq @ya-yq @ya-nf @ya-yi @nf)))

(parallel-count-paths "start-YA\nps-yq\nzt-mu\nJS-yi\nyq-VJ\nQT-ps\nstart-yq\nYA-yi\nstart-nf\nnf-YA\nnf-JS\nJS-ez\nyq-JS\nps-JS\nps-yi\nyq-nf\nQT-yi\nend-QT\nnf-yi\nzt-QT\nend-ez\nyq-YA\nend-JS\n" valid-path-v2?)


(deftest counting-paths
  (testing "simple graph"
    (is (= 10 (count-paths "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n" valid-path?))))
  (testing "more complex graph"
    (is (= 19 (count-paths "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n" valid-path?)))))

(deftest counting-paths-v2
  (testing "simple graph"
    (is (= 36 (count-paths "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n" valid-path-v2?))))
  (testing "more complex graph"
    (is (= 103 (count-paths "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n" valid-path-v2?)))))