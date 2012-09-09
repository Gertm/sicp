#lang racket

(provide (all-defined-out))

( define (average x y)
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

(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; ex1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a (* x x)) (* b x) c)))

;; ex1.41
(define (inc i) (+ i 1))

(define (double f)
  (lambda (x) (f (f x))))
;; (((double (double double)) inc) 5) => 21

;; ex1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; ex1.43
(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

(define (brepeated f n)
  (define (helper f n result)
    (if (= n 0)
        result
        (helper f (- n 1) (compose f result))))
  (helper f n (lambda (x) x)))

;; these two are more of the same, I'm skipping these.
;; ex1.44
;; ex1.45

;; this one is interesting.
;; ex1.46

;; from 1.1.7:
;; (define (sqrt-iter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x)
;;                  x)))

(define (imperative-improve good-enough? improve)
  (lambda (guess x)
    (define (helper guess x)
      (if (good-enough? guess x)
          guess
          (helper (improve guess x) x)))
    (helper guess x)))


(define (sqrt-iter guess x)
  ((imperative-improve (lambda (guess x) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess x) (average guess (/ x guess)))) guess x))

;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2)) tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))

;; so this basicly says: is the fixed point close enough?
(define (fxd-point f first-guess)
  ((imperative-improve f close-enough?) first-guess))
