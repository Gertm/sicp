#lang racket

(provide (all-defined-out))

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)


;; Exercise 1.35.  Show that the golden ratio (section 1.2.2) is a fixed point
;; of the transformation x |-> 1 + 1/x, and use this fact to compute by means of
;; the fixed-point procedure.

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.5)

(define (a) (display "got a!") (newline))

;; Exercise 1.36.  Modify fixed-point so that it prints the sequence of
;; approximations it generates, using the newline and display primitives shown
;; in exercise 1.22. Then find a solution to x^x = 1000 by finding a fixed
;; point of x |-> log(1000)/log(x). (Use Scheme's primitive log procedure, which
;; computes natural logarithms.)
;; Compare the number of steps this takes with and without average damping.
;; (Note that you cannot start fixed-point with a guess of 1, as this would
;; cause division by log(1) = 0.)

(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "Finding fixed-point for 1.36")
(newline)
(verbose-fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
(display "Finding fixed-point with average damping for 1.36")
(newline)
(verbose-fixed-point (lambda (x) (/  (+ x (/ (log 1000) (log x))) 2)) 2)
;; with average damping, this is a lot faster.



;; Exercise 1.37.  a. An infinite continued fraction is an expression of the form

;; As an example, one can show that the infinite continued fraction expansion with the Ni and the Di all equal to 1 produces 1/, where is the golden ratio (described in section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called k-term finite continued fraction -- has the form

;; Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/ using

;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            k)

;; for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?

(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           5)

;; we need to turn the formula 'inside-out' and start from the case for which we have a solution.
;; Nx and Dx, from there on it can be converted to an iterative solution.
 (define (iter-cont-frac n d k) 
   (define (iter result i) 
     (if (= i 0) 
         result 
         (iter (/ (n i)
                  (+ (d i) result))
               (- i 1)))) 
   (iter 0 k)) 


