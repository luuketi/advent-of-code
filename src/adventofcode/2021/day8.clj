(ns adventofcode.2021.day8
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def output-digits
  (->> input
       (str/split-lines)
       (map #(str/split % #"\|"))
       (map #(str/trim (nth % 1)))
       (map #(str/split % #" "))
       (flatten)))

(defn count-unique-number-of-segments [digits]
  (let [wanted-digits [2 4 3 7]]
    (->> digits
         (map #(count %))
         (filter #(some #{%} wanted-digits))
         (count))))

(defn signals-and-digits [input]
  (letfn [(to-keywords [signals] (for [s signals] (keyword (str s))))]
    (let [sd (->> input (str/split-lines) (map #(str/split % #"\ \|\ ")))]
      (for [[signals digits] sd]
        (for [a [signals digits]]
          (map #(into #{} %) (map to-keywords (str/split a #" "))))))))

(defn- filter-signals-by-lengths [signal-patterns lengths]
  (->> signal-patterns
       (filter #(some #{(count %)} lengths))
       (sort-by count)))

(defn- filter-set-by-subsets [to-filter conditions-and-subsets]
  (loop [[[cond subset] & r] conditions-and-subsets, result to-filter]
    (let [filtered (filter #(cond (set/subset? subset %)) result)]
      (if (nil? r) (first filtered) (recur r filtered)))))

(defn- signals-by-numbers [signal-patterns]
  (let [one-seven-four-eight (filter-signals-by-lengths signal-patterns [2 4 3 7])
        zero-six-nine (filter-signals-by-lengths signal-patterns [6])
        two-five-three (filter-signals-by-lengths signal-patterns [5])
        one (nth one-seven-four-eight 0)
        four (nth one-seven-four-eight 2)
        seven (nth one-seven-four-eight 1)
        eight (nth one-seven-four-eight 3)
        six (filter-set-by-subsets zero-six-nine [[false? one]])
        zero (filter-set-by-subsets zero-six-nine [[false? four] [true? one]])
        nine (filter-set-by-subsets zero-six-nine [[false? six] [false? zero]])
        wire-1 (set/difference one six)
        three (filter-set-by-subsets two-five-three [[true? one]])
        two (filter-set-by-subsets two-five-three [[false? three] [true? wire-1]])
        five (filter-set-by-subsets two-five-three [[false? three] [false? two]])]
    {zero "0", one "1", two "2", three "3", four "4", five "5", six "6", seven "7", eight "8", nine "9"}))

(defn sum-all-output-digits [input]
  (apply + (for [[signals digits] (signals-and-digits input)]
             (->> (for [d digits] (get (signals-by-numbers signals) d))
                  (apply str)
                  (Integer/parseInt)))))


(deftest test-day-8
  (testing "count-unique-number-of-segments "
    (is (= 26 (count-unique-number-of-segments output-digits))))
  (testing "sum-all "
    (is (= 61229 (sum-all-output-digits input))))
  )



