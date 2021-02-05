(def ones (cons 1 (lazy-seq ones)))
(println "ones:" (take 10 ones))

(defn ints-from [n]
  (cons n (lazy-seq (ints-from (inc n)))))

(println "ints-from" (take 10 (ints-from 10)))

(def integers (ints-from 1))

(defn divisible? 
  ([n d] (zero? (rem n d)))
  ([n d & [e more]] (or (divisible? n d) (apply divisible? n more))))

(println "div 5 3" (divisible? 5 3))
(println "div 15 3,2,5" (divisible? 15 3 2))

(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not (divisible? % (first s))) s)))))

(def primes (sieve (ints-from 2)))

(println "primes:" (take 40 primes))

(defn pairs [s t]
  (cons [(first s) (first t)]
        (lazy-seq
          (interleave
           (map #(vector (first s) %) (rest t))
           (pairs (rest s) (rest t))))))

(println "pairs:" (take 20 (pairs integers integers)))

(defn all-pairs [s t]
  (cons [(first s) (first t)]
        (lazy-seq
          (interleave
           (map #(vector (first s) %) (rest t))
           (map #(vector % (first t)) (rest s))
           (pairs (rest s) (rest t))))))

(println "all-pairs:" (take 20 (all-pairs integers integers)))

(defn triples [s t u]
  (cons (map first [s t u])
        (lazy-seq
          (interleave
            (map #(cons (first s) %) (rest (pairs t u)))
            (triples (rest s) (rest t) (rest u))))))

(println "triples:" (take 20 (triples integers integers integers)))

(def pythagoras 
  (filter (fn[[x y z]] (= (* z z) (+ (* x x) (* y y))))
          (triples integers integers integers)))

(println "pythagoras:" (take 6  pythagoras))

(defn merge-weighted [s t w]
  (let [s0 (first s)
        t0 (first t)]
    (if (<= (w s0) (w t0))
      (cons s0 (lazy-seq (merge-weighted (rest s) t w)))
      (cons t0 (lazy-seq (merge-weighted s (rest t) w))))))

(defn ordered-pairs [s t w]
  (cons [(first s) (first t)]
        (lazy-seq
          (merge-weighted
           (map #(vector (first s) %) (rest t)) 
           (ordered-pairs (rest s) (rest t) w)
           w))))

(println "ordered pairs by sum:" (time (take 10 (ordered-pairs integers integers #(reduce + %)))))

(def pairs-not-div-by-2-3-5 
  (filter (fn[[i j]] (some #(some divisible?  ) [i j])))



