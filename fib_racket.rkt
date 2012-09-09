#lang racket

(define (fib-iter n)
  (define (helper a b count)
    (if (= count 0)
        b
        (helper (+ a b) a (- count 1))))
  (helper 1 0 n))

(fib-iter 100000)
