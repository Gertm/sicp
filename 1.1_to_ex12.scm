(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; Exercise 1.1

(define a 3)
(define b (+ a 1))

;; Ex 1.2

;; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; Ex 1.3

(define (g a b c)
  (cond ((and (>= a c) (>= b c))
         (sum-of-squares a b))
        ((and (>= b a) (>= c a))
         (sum-of-squares c b))
        ((and (>= c b) (>= a b))
         (sum-of-squares c b))))

;; Ex 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Ex 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))


;; 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Ex 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; 25/05/2012

;; Ex 1.7

(define (good-enough-better? new-guess old-guess x)
  (< (abs (- new-guess old-guess)) (* 0.0001 new-guess)))


(define (sqrt-iter-better new-guess old-guess x)
  (if (good-enough-better? new-guess old-guess x)
      new-guess
      (sqrt-iter-better (improve new-guess x) new-guess x)))

(define (sqrt-better x)
  (sqrt-iter-better 1.0 1.1 x))


;; Ex 1.8

(define (cbrt x)
  (cbrt-iter 1 x))

(define (cbrt-iter guess x)
  (if (cbrt-goodenough? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x) x)))

(define (cbrt-goodenough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (improve-cbrt guess x)
  (/ (+ (* 2 guess) (/ x (* guess guess))) 3))


;; 27/05/2012

;; Ex. 1.9

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))
;; this first one is a recursive process because inc needs to wait on the result of the +


;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))
;; this is an iterative process because the + has all the state it needs.

;; Ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10)
;; (A 0 (A 1 9)) -> (* 2 (A 1 9)) -> (* 2 (A 0 (A 1 8))) -> 2 * 2 * 2 * 2 *2 *2*2*2*2*2 = 2^10 ?

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 0 (A 1 (A 2 2)))
;; (* 2 (A 1 (A 1 (A 2 1))))
;; (* 2 (A 1 (A 1 2)))
;; (* 2 (A 1 (A 0 (A 1 1))))
;; (* 2 (A 1 (A 0 2)))
;; (* 2 (A 1 (A

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-iter n)
  (define (helper a b count)
    (if (= count 0)
        b
        (helper (+ a b) a (- count 1))))
  (helper 1 0 n))

;; Ex 1.11

(define (f_rec n)
  (cond ((< n 3) n)
        (else
         (+ (f_rec (- n 1))
            (* 2 (f_rec (- n 2)))
            (* 3 (f_rec (- n 3)))))))

(define (f_iter n)
  (define (helper a b c count)
    (if (< count 3)
        a
        (helper (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (if (< n 3) n
      (helper 2 1 0 n)))


;; Ex. 1.12

(define (pascal_t row index)
  (cond ((or (= index 1)
             (= index row))
         1)
        (else
         (+ (pascal_t (- row 1) (- index 1))
            (pascal_t (- row 1) index)))))

