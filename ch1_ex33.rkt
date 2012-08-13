#lang racket

(provide (all-defined-out))
;; Exercise 1.33.  You can obtain an even more general version of accumulate
;; (exercise 1.32) by introducing the notion of a filter on the terms to be
;; combined. That is, combine only those terms derived from values in the range
;; that satisfy a specified condition. The resulting filtered-accumulate
;; abstraction takes the same arguments as accumulate, together with an additional
;; predicate of one argument that specifies the filter. Write filtered-accumulate
;; as a procedure. Show how to express the following using filtered-accumulate:

;; a. the sum of the squares of the prime numbers in the interval a to b
;; (assuming that you have a prime? predicate already written)

;; b. the product of all the positive integers less than n that are relatively
;; prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accu-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (filtered-accumulate passes-filter? combiner null-value term a next b)
  (define (iter a result)
    (if (and (> a b) (passes-filter? a))
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (inc i) (+ i 1))

(define (square i) (* i i))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? * 1 square a inc b))

;; b is more of the same, had enough for a bit.

