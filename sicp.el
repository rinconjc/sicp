(+ 2 3)
(defun add ( x  y)
  (+ x y))

(add 455 677)

(defun fibo (n)
  (defun iter (a b n)
    (if (=  0 n)
        a
      (iter b (+ a b) (- n 1))))
  (iter 0 1 n))

(assert (= 0 (fibo 0)))
(assert (= 1 (fibo 1)))
(assert (= 1 (fibo 2)))
(assert (= 2 (fibo 3)))
(assert (= 3 (fibo 4)))

(defun smallest-divisor (n)
  (defun min-divisor (a b)
    (cond
     ((> (* b b) a) a)
     ((= (mod a b) 0) b)
     (t (min-divisor a (+ b 1)))))
  (min-divisor n 2))

(assert (= 2 (smallest-divisor 200)))
(assert (= 37 (smallest-divisor 37)))
(assert (= 2 (smallest-divisor 38)))
(assert (= 3 (smallest-divisor 111)))

(message "results:%s" (mapcar 'smallest-divisor '(199 1999 19999) ))

(defun prime? (n)
  (= n (smallest-divisor n)))

(assert (every 'prime? '(2 3 5 7 11 13 17 19 23) ))

(message "time:%s" (float-time (time-subtract (current-time) (current-time))))

(defun print-elapsed (start end)
  (message "elapsed: %s" (float-time
                          (time-subtract end start))))

(defun timed-prime? (n)
  (defun find-prime (n start)
    (if (prime? n)
        (print-elapsed start (current-time))))
  (find-prime n (current-time)))

(timed-prime? 1009)

(defun search-primes (x n)
  (if (< 0 n)
      (if (timed-prime? x)
          (search-primes (+ 2 x) (- n 1))
        (search-primes (+ 2 x) n))))

;;(search-primes 1001 3)
;; (search-primes 10001 3)
;; (search-primes 100001 3)

(defun fx-integral (f a b n)
  (let ((h (/ (- b a) n)))
    (let ((hk (lambda (k)
                (funcall f (+ a (* k h)))))
          (sum-int (lambda (s k)
                     (cond
                      ((= k n) (+ s (funcall hk k)))
                      ((= k 0) (funcall sum-int (+ s (funcall hk k)) (+ k 1)))
                      ((= 0 (mod k 2)) (funcall sum-int (+ s (* 2 (funcall hk k))) (+ k 1)))
                      (t (funcall sum-int (+ s (* 4 (funcall hk k))) (+ k 1)))))))
      (/ (* h (funcall sum-int 0 0)) 3))))

(defun cube (x)
  (* x x x))

(message "integral cube: %s" (fx-integral 'cube 0 1.0 100))

(let ((a (+ 1 2))) (let ((b (* 3 a))) (+ a b)))
(funcall '+ 2 3)

(cube 2)
(let ((a 3) (f 'cube)) (funcall f (+ a 2)))
(/ 1.0 100)
