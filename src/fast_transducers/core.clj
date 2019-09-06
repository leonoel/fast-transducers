(ns fast-transducers.core
  (:refer-clojure :exclude [take take-nth drop drop-while partition-by
                            keep-indexed map-indexed distinct interpose dedupe])
  (:import (clojure.lang Box)
           (java.util ArrayList)))

(defn take [n]
  (fn [rf]
    (let [nv (Box. n)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [n  (.-val nv)
               nn (set! (.-val nv) (dec n))
               result (if (pos? n)
                        (rf result input)
                        result)]
           (if (not (pos? nn))
             (ensure-reduced result)
             result)))))))

(defn take-nth [n]
  (fn [rf]
    (let [iv (Box. -1)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [i (set! (.-val iv) (inc (.-val iv)))]
           (if (zero? (rem i n))
             (rf result input)
             result)))))))

(defn drop [n]
  (fn [rf]
    (let [nv (Box. n)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [n (.-val nv)]
           (set! (.-val nv) (dec n))
           (if (pos? n)
             result
             (rf result input))))))))

(defn drop-while [pred]
  (fn [rf]
    (let [dv (Box. true)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [drop? (.-val dv)]
           (if (and drop? (pred input))
             result
             (do
               (set! (.-val dv) nil)
               (rf result input)))))))))

(defn partition-by [f]
  (fn [rf]
    (let [a (ArrayList.)
          pv (Box. ::none)]
      (fn
        ([] (rf))
        ([result]
         (let [result (if (.isEmpty a)
                        result
                        (let [v (vec (.toArray a))]
                          ;;clear first!
                          (.clear a)
                          (unreduced (rf result v))))]
           (rf result)))
        ([result input]
         (let [pval (.-val pv)
               val (f input)]
           (set! (.-val pv) val)
           (if (or (identical? pval ::none)
                   (= val pval))
             (do
               (.add a input)
               result)
             (let [v (vec (.toArray a))]
               (.clear a)
               (let [ret (rf result v)]
                 (when-not (reduced? ret)
                   (.add a input))
                 ret)))))))))

(defn keep-indexed [f]
  (fn [rf]
    (let [iv (Box. -1)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [i (set! (.-val iv) (inc (.-val iv)))
               v (f i input)]
           (if (nil? v)
             result
             (rf result v))))))))

(defn map-indexed [f]
  (fn [rf]
    (let [i (Box. -1)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (rf result (f (set! (.-val i) (inc (.-val i))) input)))))))

(defn distinct []
  (fn [rf]
    (let [seen (Box. #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if (contains? (.-val seen) input)
           result
           (do (set! (.-val seen) (conj (.-val seen) input))
               (rf result input))))))))

(defn interpose [sep]
  (fn [rf]
    (let [started (Box. false)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if (.-val started)
           (let [sepr (rf result sep)]
             (if (reduced? sepr)
               sepr
               (rf sepr input)))
           (do
             (set! (.-val started) true)
             (rf result input))))))))

(defn dedupe []
  (fn [rf]
    (let [pv (Box. ::none)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [prior (.-val pv)]
           (set! (.-val pv) input)
           (if (= prior input)
             result
             (rf result input))))))))