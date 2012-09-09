#lang typed/racket

(: fib-iter (Integer -> Integer))
(define (fib-iter n)
  (: helper (Integer Integer Integer -> Integer))
  (define (helper a b count)
    (if (= count 0)
        b
        (helper (+ a b) a (- count 1))))
  (helper 1 0 n))

(fib-iter 10000)
