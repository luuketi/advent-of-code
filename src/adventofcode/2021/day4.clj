(ns adventofcode.2021.day4
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))


(def numbers [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])

(def boards-input "22 13 17 11  0
                   8  2 23  4 24
                   21  9 14 16  7
                   6 10  3 18  5
                   1 12 20 15 19

                   3 15  0  2 22
                   9 18 13 17  5
                   19  8  7 25 23
                   20 11 10 24  4
                   14 21 16 12  6

                   14 21 17 24  4
                   10 16 15  9 19
                   18  8 23 26 20
                   22 11 13  6  5
                   2  0 12  3  7")

(def boards
  (for [boards (for [lines (str/split boards-input #"\n\n")] (str/split-lines lines))]
    (vec (for [lines (for [line boards] (filter #(not= "" %) (str/split line #" ")))]
      (vec (for [char lines] (Integer/parseInt char)))))))

(defn check-wins [board]
  (let [win-row [nil nil nil nil nil]
        columns (for [i (range 5)] (for [line board] (nth line i)))]
    (letfn [(has-win-row [board] (not (nil? (some #{win-row} board))))]
      (or (has-win-row board) (has-win-row columns)))))

(defn mark-board [board number]
  (for [line board]
    (if (nil? (some #{number} line))
      line
      (assoc line (.indexOf line number) nil))))

(defn sum-board [board]
  (apply + (for [line board] (->> line (filter identity) (apply +)))))

(defn score-game [boards numbers]
  (loop [[number & r] numbers, boards boards]
    (let [new-boards (for [board boards] (mark-board board number))
          winner-board (first (filter #(check-wins %) new-boards))]
      (if (and (nil? winner-board) (not (empty? r)))
        (recur r new-boards)
        (* number (sum-board winner-board))))))


(defn get-last-winner [winners]
  (if (empty? winners)
    [0 0]
    (last winners)))

(defn last-winner-score [boards numbers]
  (loop [[number & r] numbers, boards boards, winners []]
    (let [new-boards (for [board boards] (mark-board board number))
          winner-boards (filter #(check-wins %) new-boards)
          non-winner-boards (remove #(some #{%} winner-boards) new-boards)]
      (if (nil? r)
        (let [[winner-number winner-board] (get-last-winner winners)]
          (* winner-number (sum-board winner-board)))
        (if (empty? winner-boards)
          (recur r non-winner-boards winners)
          (recur r non-winner-boards (conj winners [number (first winner-boards)])))))))
(last-winner-score boards numbers)

(deftest test-sonar-sweep
  (testing "check win board "
    (is (= true (check-wins [[1 2 3 4 5][1 2 3 4 5][1 2 3 4 5][1 2 3 4 5] [ nil nil nil nil nil]])))
    (is (= true (check-wins [[nil 2 3 4 5][nil 2 3 4 5][nil 2 3 4 5][nil 2 3 4 5] [ nil 2 3 4 5]])))
    (is (= false (check-wins [[1 2 3 4 5][3 2 3 4 5][7 2 3 4 5][7 2 3 4 5] [ 7 2 3 4 5]]))))
  (testing "check score"
    (is ( = 4512 (score-game boards numbers))))
  (testing "check last winner board"
    (is ( = 1924 (last-winner-score boards numbers))))
  )

