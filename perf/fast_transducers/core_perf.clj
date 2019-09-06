(ns fast-transducers.core-perf
  (:require [criterium.core :as c]
            [fast-transducers.core]))

(defmacro measure [& body]
  `(-> (c/quick-benchmark (do ~@body) {}) :mean first))

(defn print-improvement [n c f]
  (println n (-> (- c f) (* 100) (/ c) (str "%"))))

(defmacro bench [sym f]
  `(print-improvement ~(name sym)
                      (let ~[sym (symbol "clojure.core" (name sym))] (measure ~f))
                      (let ~[sym (symbol "fast-transducers.core" (name sym))] (measure ~f))))

(defn -main [& _]
  (let [input (vec (shuffle (mapcat range (range 100))))]
    (bench take (transduce (take 100) {} nil input))
    (bench take-nth (transduce (take-nth 100) {} nil input))
    (bench drop (transduce (drop 100) {} nil input))
    (bench drop-while (transduce (drop-while (partial > 15)) {} nil input))
    (bench partition-by (transduce (partition-by #(zero? (mod % 5))) {} nil input))
    (bench keep-indexed (transduce (keep-indexed (fn [i x] (when (zero? (mod i 3)) x))) {} nil input))
    (bench map-indexed (transduce (map-indexed (fn [i x] (when (zero? (mod i 3)) x))) {} nil input))
    (bench distinct (transduce (distinct) {} nil input))
    (bench interpose (transduce (interpose nil) {} nil input))
    (bench dedupe (transduce (dedupe) {} nil input))))