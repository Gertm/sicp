#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (inc a)
  (+ a 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (simpson f a b n)
  (define (simpsum h)
    (define (y k)
      (f (+ a (* k h))))
    (define (multiplicator k)
      (cond ((or (= k 0) (= k n)) (y k))
            ((even? k) (* 2 (y k)))
            (else (* 4 (y k)))))
    (* (sum multipicator 0 inc n)
       (/ h 3)))
  (simpsum (/ (- b a) n))

(simpson cube 0 1 100))

(simpson cube 0 1 1000)

(integral cube 0 1 0.01)

(integral cube 0 1 0.001)

