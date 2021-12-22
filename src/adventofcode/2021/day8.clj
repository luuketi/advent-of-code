(ns adventofcode.2021.day8
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))

(def input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def output-digits
  (->> input
       (str/split-lines)
       (map #(str/split % #"\|"))
       (map #(str/trim (nth % 1)))
       (map #(str/split % #" "))
       (flatten)
       ))

(defn count-unique-number-of-segments [digits]
  (let [wanted-digits [2 4 3 7]]
    (->> digits
         (map #(count %))
         (filter #(some #{%} wanted-digits))
         (count))))

(def signal-patterns
  {(apply str (sort "acedgfb")) 8,
   (apply str (sort "cdfbe"))   5,
   ;(apply str (sort "gcdfa"))   2,
   ;(apply str (sort "fbcad"))   3,
   ;(apply str (sort "dab"))     7,
   (apply str (sort "cefabd"))  9,
   (apply str (sort "cdfgeb"))  6,
   ;(apply str (sort "eafb"))    4,
   (apply str (sort "cagedb"))  0,
   (apply str (sort "ab"))      1}
  )

(def output-digits
  (->> input
       (str/split-lines)
       (map #(str/split % #"\|"))
       (map #(str/trim (nth % 1)))
       (map #(str/split % #" "))
       ))


(defn count-segments [digits]
  (let [unique-digits {2 1, 4 4, 3 7, 7 8}
        numbers (for [d digits]
               (if (nil? (get signal-patterns (apply str (sort d))))
                 (get unique-digits (count d))
                 (get signal-patterns (apply str (sort d)))))]
    (Integer/parseInt (apply str numbers)))
  )

(defn sum-all-segments [digits]
  (for [d digits] (count-segments d)))

(deftest test-count-unique-number-of-segments
  (testing "count-unique-number-of-segments "
    (is (= 26 (count-unique-number-of-segments output-digits))))
  (testing "count-segments "
    (is (= 5353 (count-segments ["cdfeb" "fcadb" "cdfeb" "cdbaf"]))))
  )



