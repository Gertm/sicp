#lang racket

(provide (all-defined-out))

;; Exercise 1.32.  a. Show that sum and product (exercise 1.31) are both special
;; cases of a still more general notion called accumulate that combines a
;; collection of terms, using some general accumulation function:

;; (accumulate combiner null-value term a next b)

;; Accumulate takes as arguments the same term and range specifications as
;; sum and product, together with a combiner procedure (of two arguments) that
;; specifies how the current term is to be combined with the accumulation of
;; the preceding terms and a null-value that specifies what base value to use
;; when the terms run out. Write accumulate and show how sum and product can
;; both be defined as simple calls to accumulate.

;; b. If your accumulate procedure generates a recursive process, write one
;; that generates an iterative process. If it generates an iterative process,
;; write one that generates a recursive process.

;; (define (product term a next b)
;;   (if (> a b)
;;       1
;;       (* (term a)
;;          (product term (next a) next b))))

(define (inc i) (+ i 1))

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

(define (iterprod term a next b)
  (accu-iter * 1 term a next b))

(define (iterpi4 i)
  (define (fraction n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (iterprod fraction 1 inc i))

