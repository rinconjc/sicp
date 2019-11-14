(println "hello world")
(defn sum [term a next b]
  (loop [x a
         s 0]
    (if (> a b) s
        (recur (next a) (+ s (term a))))))

(defn integral [f a b n])
