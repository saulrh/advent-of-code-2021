(ns aoc-test
  (:require
   [clojure.test :refer :all]
   [aoc :refer :all]))

(deftest test-in-rect
  (is (in (->Rectangle (->Vector2d 2 -6)
                       (->Vector2d 4 -10))
          (->Vector2d 3 -8))))

(deftest test-add-two-vectors
  (is (= (add (->Vector2d 1 3)
              (->Vector2d 4 -1))
         (->Vector2d 5 2))))

(deftest test-add-num-to-vector
  (is (= (add (->Vector2d 1 3)
              5)
         (->Vector2d 6 8))))

(deftest test-add-vector-to-num
  (is (= (add 5
              (->Vector2d 4 -1))
         (->Vector2d 9 4))))

(deftest test-vector-neg
  (is (= (neg (->Vector2d 1 3))
         (->Vector2d -1 -3))))

(deftest test-vector-sign
  (is (= (sign (->Vector2d 1 -3))
         (->Vector2d 1 -1)))
  (is (= (sign (->Vector2d 0 2))
         (->Vector2d 0 1))))

(deftest test-occupied-pts-seq
  (is (= (occupied-pts-seq EXAMPLE_TARGET (->Vector2d 7 2))
         (list (->Probe (->Vector2d 0 0) (->Vector2d 7 2))
               (->Probe (->Vector2d 7 2) (->Vector2d 6 1))
               (->Probe (->Vector2d 13 3) (->Vector2d 5 0))
               (->Probe (->Vector2d 18 3) (->Vector2d 4 -1))
               (->Probe (->Vector2d 22 2) (->Vector2d 3 -2))
               (->Probe (->Vector2d 25 0) (->Vector2d 2 -3))
               (->Probe (->Vector2d 27 -3) (->Vector2d 1 -4))
               (->Probe (->Vector2d 28 -7) (->Vector2d 0 -5))))))

(deftest test-first-in-target
  (is (= (first-in-target EXAMPLE_TARGET (->Vector2d 7 2))
         (->Probe (->Vector2d 28 -7) (->Vector2d 0 -5)))))

(deftest test-hits
  (is (every? 
       #(not (nil? (first-in-target EXAMPLE_TARGET %)))
       [(->Vector2d 6 3)
        (->Vector2d 7 2)
        (->Vector2d 9 0)])))

(deftest test-misses
  (is (every? 
       #(nil? (first-in-target EXAMPLE_TARGET %))
       [(->Vector2d 17 -4)])))


(deftest test-max-over
  (is (= (max-over #(* % % %) [1 -2 3 -4 5])
         125)))

(deftest test-y-points
  (is (= (map #(-> % .pos .y) (occupied-pts-seq EXAMPLE_TARGET (->Vector2d 6 9)))
         '(0 9 17 24 30 35 39 42 44 45 45 44 42 39 35 30 24 17 9 0 -10))))

(deftest test-highest-point
  (is (= (highest-point EXAMPLE_TARGET (->Vector2d 6 9))
         45)))

(deftest test-seq-len
  (is (= (seq-len (range 10))
         10)))

(deftest test-part1-example
  (is (= (part1 EXAMPLE_TARGET) 45)))

(deftest test-part2
  (is (= (part2 EXAMPLE_TARGET) 112)))
