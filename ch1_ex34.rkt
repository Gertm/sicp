#lang racket

(provide (all-defined-out))

;; Exercise 1.34.  Suppose we define the procedure

;; (define (f g)
;;   (g 2))

;; Then we have

;; (f square)
;; 4

;; (f (lambda (z) (* z (+ z 1))))
;; 6

;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

(define (square x) (* x x))

(define (f g)
  (g 2))

;; (f f) => (f 2) => 2 is not a procedure. It'll try to call '2' as a function, which it is not.
