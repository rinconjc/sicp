(def ones (cons 1 (lazy-seq ones)))
(println "ones:" (take 10 ones))

(defn ints-from [n]
  (cons n (lazy-seq (ints-from (inc n)))))

(println "ints-from" (take 10 (ints-from 10)))

(def integers (ints-from 1))

(defn divisible? [n d]
  (zero? (rem n d)))

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
