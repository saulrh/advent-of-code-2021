(ns aoc)

(defrecord Vector2d [x y])

(defmacro implement-op-for-vector
  [name op]
  `(do
     (defmulti ~name (fn [a# b#] [(instance? Vector2d a#) (instance? Vector2d b#)]))
     (defmethod ~name [true true] [va# vb#]
       (->Vector2d (~op (.x va#) (.x vb#))
                   (~op (.y va#) (.y vb#))))
     (defmethod ~name [true false] [va# nb#]
       (->Vector2d (~op (.x va#) nb#)
                   (~op (.y va#) nb#)))
     (defmethod ~name [false true] [va# vb#] (~name vb# va#))
     (defmethod ~name [false false] [na# nb#] (~op na# nb#))))

(implement-op-for-vector add +)
(implement-op-for-vector sub -)
(implement-op-for-vector mul *)
(implement-op-for-vector div /)

(defn neg [v] (->Vector2d (- (.x v))
                          (- (.y v))))

(defmulti sign (fn [a#] (instance? Vector2d a#)))
(defmethod sign false [n] (if (= 0 n)
                            0
                            (/ n (Math/abs n))))
(defmethod sign true [v] (->Vector2d (sign (.x v))
                                     (sign (.y v))))

(defn above-left [a b]
  (and (<= (.x a) (.x b))
       (>= (.y a) (.y b))))


(defrecord Rectangle [tl br])

(defn in [rect pt]
  (and (>= (.x pt) (.x (.tl rect)))
       (<= (.y pt) (.y (.tl rect)))
       (<= (.x pt) (.x (.br rect)))
       (>= (.y pt) (.y (.br rect)))))

(defn between? [lower upper n]
  (and (lower < n)
       (upper > n)))

(defn between=? [lower upper n]
  (and (lower <= n)
       (upper >= n)))

(defn past [rect pt]
  (or (> (.x pt) (.x (.br rect)))
      (< (.y pt) (.y (.br rect)))))

(defrecord Probe [pos vel])

(defn step [probe]
  (let [px (.x (.vel probe))
        py (.y (.vel probe))]
    (->Probe (add (.pos probe) (.vel probe))
             (->Vector2d (- px (sign px))
                         (- py 1)))))

(defn occupied-pts-seq [target-rect velocity]
  (->>
   (iterate step (->Probe (->Vector2d 0 0) velocity))
   (take-while #(not (past target-rect (.pos %))))))

(defn first-in-target [target-rect velocity]
  (->>
   (occupied-pts-seq target-rect velocity)
   (filter #(in target-rect (.pos %)))
   (first)))

(defn max-over [fn coll]
  (reduce max (map fn coll)))

(defn highest-point [target-rect velocity]
  (->>
   (occupied-pts-seq target-rect velocity)
   (max-over #(-> % .pos .y))))

(defn velocities [target-rect]
  (for [y (range (-> target-rect .br .y dec)
                 (-> target-rect .br .y - inc))
        x (range 0
                 (-> target-rect .br .x inc))]
    (->Vector2d x y)))

(defn part1 [target-rect]
  (->>
   (velocities target-rect)
   (filter #(first-in-target target-rect %))
   (map #(highest-point target-rect %))
   (reduce max)))

(defn seq-len [seq]
  (->>
   (map (fn [_] 1) seq)
   (reduce +)))

(defn part2 [target-rect]
  (->>
   (velocities target-rect)
   (map #(first-in-target target-rect %))
   (remove nil?)
   (seq-len)))

(def EXAMPLE_TARGET (->Rectangle (->Vector2d 20 -5) (->Vector2d 30 -10)))
(def REAL_TARGET (->Rectangle (->Vector2d 32 -177) (->Vector2d 65 -225)))

(defn run [opts]
  (println (part1 REAL_TARGET))
  (println (part2 REAL_TARGET)))
