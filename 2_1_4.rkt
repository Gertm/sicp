#lang racket

(provide (all-defined-out))

(define (make-interval x y)
  (cons x y))

;; ex 2.7
(define (upper-bound i)
  (max (car i) (cdr i)))

(define (lower-bound i)
  (min (car i) (cdr i)))

;; the min/max functions are needed to figure out what the actual
;; lowest values are. The way you make the interval
;; never specifies what needs to come first..
;; you can do (make-interval 3 9) and (make-interval 5 2)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Interval y spans 0!")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;; ex 2.9

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; adding 2 intervals with a certain width
;; adding can be expressed in terms of substraction so it's the same for that.
;; results in the same width every time:
(width (add-interval (make-interval 5 1) (make-interval 6 2)))
;; =
(width (add-interval (make-interval 7 3) (make-interval 4 8)))

;; but with multiplication this won't be the case:
;; (division is expressed in terms of multipication so it's the same)
(width (mul-interval (make-interval 5 1) (make-interval 6 2)))
;; /=
(width (mul-interval (make-interval 7 3) (make-interval 4 8)))

;; ex 2.10
;; see the division function ^

;; ex 2.11

;; In passing, Ben also cryptically comments: ``By testing the signs
;; of the endpoints of the intervals, it is possible to break mul-interval
;; into nine cases, only one of which requires more than two multiplications.''
;; Rewrite this procedure using Ben's suggestion.

;; UGH!

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))


;; ex 2.12
;; Define a constructor make-center-percent that takes a center and a
;; percentage tolerance and produces the desired interval. You must also
;; define a selector percent that produces the percentage tolerance for
;; a given interval. The center selector is the same as the one shown above.

(define (make-center-percent c p)
  (let ((width (/ (* c p) 100.0)))
    (make-interval (- c width) (+ c width))))

(define (percent interval)
  (/ (* (width interval) 100) (center interval)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
