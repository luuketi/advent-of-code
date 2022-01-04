(ns adventofcode.2021.day10
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))

(def input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")

(defn chunks [input]
  (let [splitted (->> input (str/split-lines) (map #(str/split % #"")))]
    (for [s splitted] (map keyword s))))

(def legal-pairs
  {(keyword ")") (keyword "("),
   (keyword "]") (keyword "["),
   (keyword "}") (keyword "{"),
   (keyword ">") (keyword "<")})

(def legal-start-char [(keyword "(") (keyword "[") (keyword "{") (keyword "<")])

(def chars-and-points
  {(keyword ")") 3,
   (keyword "]") 57,
   (keyword "}") 1197,
   (keyword ">") 25137})

(defn syntax-error-score [input]
  (letfn [(is-legal-closing-char [char stack] (= (legal-pairs char) (peek stack)))]
    (apply + (for [line (chunks input)]
               (loop [[char & r] line, stack []]
                 (if (some #{char} legal-start-char)
                   (if (nil? r) 0 (recur r (into stack [char])))
                   (if (is-legal-closing-char char stack)
                     (if (nil? r) 0 (recur r (pop stack)))
                     (chars-and-points char))))))))

(defn- incomplete-chars [line]
  (letfn [(is-legal-open-char [char] (some #{char} legal-start-char))
          (is-legal-closing-char [char stack] (= (legal-pairs char) (peek stack)))]
    (loop [[char & r] line, stack []]
      (if (is-legal-open-char char)
        (if (nil? r) (into stack [char]) (recur r (into stack [char])))
        (when (is-legal-closing-char char stack)
          (if (nil? r) (pop stack) (recur r (pop stack))))))))

(def inverted-legal-pairs (into {} (for [[k v] legal-pairs] [v k])))

(def incomplete-char-points {(keyword ")") 1, (keyword "]") 2, (keyword "}") 3, (keyword ">") 4})

(defn incomplete-lines-score [input]
  (letfn [(score [res val] (+ (* 5 res) (incomplete-char-points val)))]
    (let [pending-chars (for [line (chunks input)]
                          (->> (incomplete-chars line)
                               (map #(inverted-legal-pairs %))
                               (reverse)))
          scores (->> pending-chars (map #(reduce score 0 %)) (sort) (filter pos-int?))]
      (nth scores (quot (count scores) 2)))))

(deftest syntax-error-scores
  (testing "}"
    (is (= 1197 (syntax-error-score "{([(<{}[<>[]}>{[]{[(<()>"))))
  (testing ")"
    (is (= 3 (syntax-error-score "[[<[([]))<([[{}[[()]]]"))))
  (testing "]"
    (is (= 57 (syntax-error-score "[{[{({}]{}}([{[{{{}}([]"))))
  (testing ">"
    (is (= 25137 (syntax-error-score "<{([([[(<>()){}]>(<<{{"))))
  (testing "multiple"
    (is (= 26397 (syntax-error-score input)))))

(deftest incomplete-lines-scores
  (testing "incomplete-lines-score"
    (is (= 288957 (incomplete-lines-score input)))))

