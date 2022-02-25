(ns adventofcode.2021.day16
  (:require [clojure.test :refer :all]
            [clojure.tools.trace]))


(defn- b->i [b]
  (BigInteger. ^String b 2))

(defn- unhex [s]
  (->> (partition 1 s)
       (map #(Integer/parseInt (apply str %) 16))
       (map #(Long/toBinaryString %))
       (map #(format "%04d" (Integer/parseInt %)))
       (apply str)))

(defn packet-version [stream]
  (->> stream (take 3) vec (apply str) b->i))

(defn packet-type-id [stream]
  (->> stream (take 6) (take-last 3) vec (apply str) b->i))

(defn length-type-id [stream]
  (->> stream (drop 6) first str b->i))

(defn subpackets-length [stream]
  (->> stream (drop 7) (take 15) vec (apply str) b->i))

(defn subpackets-quantity [stream]
  (->> stream (drop 7) (take 11) vec (apply str) b->i))

(defn- read-literal [stream]
  (loop [data (drop 6 stream), values [], length 0]
    (let [value (->> data (take 5) (take-last 4))]
      (if (= \0 (first data))
        (let [value (->> (into values value) (apply str) b->i)]
          {:value value :bits-read (+ 5 length)})
        (recur (drop 5 data) (into values value) (+ 5 length))))))

(defn read-literal-packet [stream]
  (let [{literal-value :value, bits-read :bits-read} (read-literal stream)
        packet-bits-read (+ 6 bits-read)]
    {:packet    {:version (packet-version stream), :values [literal-value]}
     :type      :literal
     :bits-read packet-bits-read
     :rest      (drop packet-bits-read stream)}))


(declare read-packet)

(defn- read-operator-by-length-type-id [stream prefix-length stop-f]
  (loop [stream (drop prefix-length stream), version-sum 0, total-bits-read 0, packets 0, all-values []]
    (if (stop-f total-bits-read packets)
      {:version-sum version-sum, :packet-bits-read (+ total-bits-read prefix-length), :values all-values}
      (let [{{version :version, values :values} :packet,
             bits-read                          :bits-read,
             new-rest                           :rest} (read-packet stream)]
        (recur new-rest
               (+ version-sum version)
               (+ total-bits-read bits-read)
               (inc packets)
               (into all-values values))))))


(defn- read-operator [stream]
  (if (zero? (length-type-id stream))
    (read-operator-by-length-type-id stream 22 (fn [total-bits-read packets] (= total-bits-read (subpackets-length stream))))
    (read-operator-by-length-type-id stream 18 (fn [total-bits-read packets] (= packets (subpackets-quantity stream))))))


(defn read-operator-packet [stream]
  (let [{version-num      :version-sum,
         packet-bits-read :packet-bits-read,
         values           :values} (read-operator stream)
        value (condp = (packet-type-id stream)
                0 (reduce + 0 values)
                1 (reduce * 1 values)
                2 (reduce min values)
                3 (reduce max values)
                5 ((fn [[a b]] (if (> a b) 1 0)) values)
                6 ((fn [[a b]] (if (< a b) 1 0)) values)
                7 ((fn [[a b]] (if (= a b) 1 0)) values))]
    {:packet    {:version (+ version-num (packet-version stream))
                 :values   [value]}
     :type      :operator
     :bits-read packet-bits-read
     :rest      (drop packet-bits-read stream)}))


(defn read-packet [stream]
  (if (= 4 (packet-type-id stream))
    (read-literal-packet stream)
    (read-operator-packet stream)))
(clojure.tools.trace/trace-vars adventofcode.2021.day16/read-packet)


(defn eval-packet [stream]
  (->> (read-packet stream) :packet :values first))


(deftest test-literals
  (testing "read D2FE28"
    (is (= {:packet    {:version 6, :values [2021]},
            :type      :literal,
            :bits-read 21,
            :rest      '(\0 \0 \0)}
           (read-literal-packet (unhex "D2FE28"))))))

(deftest test-operators
  (testing "read 38006F45291200"
    (is (= {:packet    {:version 9, :values [1]},
            :type      :operator,
            :bits-read 49,
            :rest      '(\0 \0 \0 \0 \0 \0 \0)}
           (read-operator-packet (unhex "38006F45291200")))))
  (testing "read EE00D40C823060"
    (is (= {:packet    {:version 14, :values [3]},
            :type      :operator,
            :bits-read 51,
            :rest      '(\0 \0 \0 \0 \0)}
           (read-operator-packet (unhex "EE00D40C823060"))))))

(deftest test-read
  (testing "read 8A004A801A8002F478"
    (is (= {:packet    {:version 16, :values [15]},
            :type      :operator,
            :bits-read 69,
            :rest      '(\0 \0 \0)}
           (read-packet (unhex "8A004A801A8002F478")))))
  (testing "read 620080001611562C8802118E34"
    (is (= {:packet    {:version 12, :values [46]},
            :type      :operator,
            :bits-read 102,
            :rest      '(\0 \0)}
           (read-packet (unhex "620080001611562C8802118E34")))))
  (testing "read C0015000016115A2E0802F182340"
    (is (= {:packet    {:version 23, :values [46]},
            :type      :operator,
            :bits-read 106,
            :rest      '(\0 \0 \0 \0 \0 \0)}
           (read-packet (unhex "C0015000016115A2E0802F182340")))))
  (testing "read A0016C880162017C3686B18A3D4780"
    (is (= {:packet    {:version 31, :values [54]},
            :type      :operator,
            :bits-read 113,
            :rest      '(\0 \0 \0 \0 \0 \0 \0)}
           (read-packet (unhex "A0016C880162017C3686B18A3D4780"))))))

(deftest test-eval-packet
  (testing "eval-packet C200B40A82"
    (is (= 3 (eval-packet (unhex "C200B40A82")))))
  (testing "eval-packet 04005AC33890"
    (is (= 54 (eval-packet (unhex "04005AC33890")))))
  (testing "eval-packet 880086C3E88112"
    (is (= 7 (eval-packet (unhex "880086C3E88112")))))
  (testing "eval-packet CE00C43D881120"
    (is (= 9 (eval-packet (unhex "CE00C43D881120")))))
  (testing "eval-packet D8005AC2A8F0"
    (is (= 1 (eval-packet (unhex "D8005AC2A8F0")))))
  (testing "eval-packet F600BC2D8F"
    (is (= 0 (eval-packet (unhex "F600BC2D8F")))))
  (testing "eval-packet 9C005AC2F8F0"
    (is (= 0 (eval-packet (unhex "9C005AC2F8F0")))))
  (testing "eval-packet 9C0141080250320F1802104A08"
    (is (= 1 (eval-packet (unhex "9C0141080250320F1802104A08"))))))

(clojure.tools.trace/trace-vars adventofcode.2021.day16/read-literal)
(clojure.tools.trace/trace-vars adventofcode.2021.day16/read-operator-packet)
(clojure.tools.trace/trace-vars adventofcode.2021.day16/read-operator)
