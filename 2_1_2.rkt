#lang racket

(provide (all-defined-out))

;; Exercise 2.2.  Consider the problem of representing line segments in a plane.
;; Each segment is represented as a pair of points: a starting point and an
;; ending point. Define a constructor make-segment and selectors start-segment
;; and end-segment that define the representation of segments in terms of
;; points. Furthermore, a point can be represented as a pair of numbers: the
;; x coordinate and the y coordinate. Accordingly, specify a constructor
;; make-point and selectors x-point and y-point that define this representation.
;; Finally, using your selectors and constructors, define a procedure
;; midpoint-segment that takes a line segment as argument and returns its
;; midpoint (the point whose coordinates are the average of the coordinates
;; of the endpoints). To try your procedures, you'll need a way to print points:

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((x (average (x-point (start-segment segment)) (x-point (end-segment segment))))
        (y (average (y-point (start-segment segment)) (y-point (end-segment segment)))))
    (make-point x y)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))


;; Exercise 2.3.  Implement a representation for rectangles in a plane.
;; (Hint: You may want to make use of exercise 2.2.) In terms of your
;; constructors and selectors, create procedures that compute the perimeter
;; and the area of a given rectangle. Now implement a different representation
;; for rectangles. Can you design your system with suitable abstraction
;; barriers, so that the same perimeter and area procedures will work using
;; either representation?

;; Make sure you have nice accessor functions on both representations,
;; the actual perimeter calculation will use those accessor functions.
;; Not typing the entire thing, confident that I get it, moving on.

