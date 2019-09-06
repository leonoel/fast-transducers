(ns fast-transducers.core-test
  (:require [clojure.test :refer :all]
            [fast-transducers.core :as ft]))

(deftest test-dedupe
  (are [x y] (= (transduce (ft/dedupe) conj x) y)
             [] []
             [1] [1]
             [1 2 3] [1 2 3]
             [1 2 3 1 2 2 1 1] [1 2 3 1 2 1]
             [1 1 1 2] [1 2]
             [1 1 1 1] [1]

             "" []
             "a" [\a]
             "aaaa" [\a]
             "aabaa" [\a \b \a]
             "abba" [\a \b \a]

             [nil nil nil] [nil]
             [1 1.0 1.0M 1N] [1 1.0 1.0M 1N]
             [0.5 0.5] [0.5]))

(deftest test-take
  (are [n y] (= (transduce (ft/take n) conj [1 2 3 4 5]) y)
             1 '(1)
             3 '(1 2 3)
             5 '(1 2 3 4 5)
             9 '(1 2 3 4 5)
             0 ()
             -1 ()
             -2 ()))

(deftest test-drop
  (are [n y] (= (transduce (ft/drop n) conj [1 2 3 4 5]) y)
             1 '(2 3 4 5)
             3 '(4 5)
             5 ()
             9 ()
             0 '(1 2 3 4 5)
             -1 '(1 2 3 4 5)
             -2 '(1 2 3 4 5)))


(deftest test-take-nth
  (are [n y] (= (transduce (ft/take-nth n) conj [1 2 3 4 5]) y)
             1 '(1 2 3 4 5)
             2 '(1 3 5)
             3 '(1 4)
             4 '(1 5)
             5 '(1)
             9 '(1)))

(deftest test-drop-while
  (are [coll y] (= (transduce (ft/drop-while pos?) conj coll) y)
                [] ()
                [1 2 3 4] ()
                [1 2 3 -1] '(-1)
                [1 -1 2 3] '(-1 2 3)
                [-1 1 2 3] '(-1 1 2 3)
                [-1 -2 -3] '(-1 -2 -3)))

(deftest test-distinct
  (are [out in] (= out (sequence (distinct in)) (sequence (ft/distinct) in))
                [] []
                (range 10) (range 10)
                [0] (repeat 10 0)
                [0 1 2] [0 0 1 1 2 2 1 1 0 0]
                [1] [1 1N]))

(deftest test-interpose
  (are [out in] (= out (sequence (ft/interpose :s) in))
                [] (range 0)
                [0] (range 1)
                [0 :s 1] (range 2)
                [0 :s 1 :s 2] (range 3))
  (testing "Can end reduction on separator or input"
    (let [expected (interpose :s (range))]
      (dotimes [i 10]
        (is (= (take i expected)
               (sequence (comp (ft/interpose :s) (take i))
                         (range))))))))

(deftest test-map-indexed
  (is (= []
         (sequence (ft/map-indexed vector) [])))
  (is (= [[0 1] [1 2] [2 3] [3 4]]
         (sequence (ft/map-indexed vector) (range 1 5)))))