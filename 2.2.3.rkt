#lang racket

(provide (all-defined-out))

(define (square x) (* x x))

(define nil '())

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; Exercise 2.33.  Fill in the missing expressions to complete the following
;; definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))


;; Exercise 2.34.  Evaluating a polynomial in x at a given value of x
;; can be formulated as an accumulation. We evaluate the polynomial

;; using a well-known algorithm called Horner's rule, which structures
;; the computation as

;; In other words, we start with an, multiply by x, add an-1, multiply
;; by x, and so on, until we reach a0 Fill in the following template
;; to produce a procedure that evaluates a polynomial using Horner's
;; rule.  Assume that the coefficients of the polynomial are arranged
;; in a sequence, from a0 through an.


;; (define (horner-eval x coefficient-sequence)
;;   (accumulate (lambda (this-coeff higher-terms) <??>)
;;               0
;;               coefficient-sequence))

;; For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would evaluate

;; (horner-eval 2 (list 1 3 0 5 0 1))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:

;; (define (count-leaves t)
;;   (accumulate <??> <??> (map <??> <??>)))
(define t '((3 4) (1 2)))

(define (count-leaves2 t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x) (count-leaves2 x)
                             1)) t)))

;; Exercise 2.36.  The procedure accumulate-n is similar to accumulate
;; except that it takes as its third argument a sequence of sequences,
;; which are all assumed to have the same number of elements. It
;; applies the designated accumulation procedure to combine all the
;; first elements of the sequences, all the second elements of the
;; sequences, and so on, and returns a sequence of the results. For
;; instance, if s is a sequence containing four sequences, ((1 2 3) (4
;; 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s)
;; should be the sequence (22 26 30). Fill in the missing expressions
;; in the following definition of accumulate-n:

;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       nil
;;       (cons (accumulate op init <??>)
;;             (accumulate-n op init <??>))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;; Had to cheat and go look at the solution.
;; Let's try to figure out why this works.
;; AHA, at the last one, the lists are (() () ()), So (car seqs) results in '() => return '()
;; and this works nicely then.
;; Mapping over them lets you build the resulting list easily.

;; Exercise 2.37.  Suppose we represent vectors v = (vi) as sequences
;; of numbers, and matrices m = (mij) as sequences of vectors (the
;; rows of the matrix). For example, the matrix
;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8
;; 9)). With this representation, we can use sequence operations to
;; concisely express the basic matrix and vector operations. These
;; operations (which are described in any book on matrix algebra) are
;; the following:

;; We can define the dot product as17

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for
;; computing the other matrix operations. (The procedure accumulate-n
;; is defined in exercise 2.36.)

;; (define (matrix-*-vector m v)
;;   (map <??> m))
;; (define (transpose mat)
;;   (accumulate-n <??> <??> mat))
;; (define (matrix-*-matrix m n)
;;   (let ((cols (transpose n)))
;;     (map <??> m)))


;; Exercise 2.38.  The accumulate procedure is also known as
;; fold-right, because it combines the first element of the sequence
;; with the result of combining all the elements to the right. There
;; is also a fold-left, which is similar to fold-right, except that it
;; combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of
(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

;; Give a property that op should satisfy to guarantee that fold-right
;; and fold-left will produce the same values for any sequence.
(fold-right + 0 '(1 2 3 4))
(fold-left + 0 '(1 2 3 4))

;; -> the operation has to be commutative
;; --> apparently, having the operation be associative works too.

;; Exercise 2.39.  Complete the following definitions of reverse
;; (exercise 2.18) in terms of fold-right and fold-left from exercise
;; 2.38:

;; (define (reverse sequence)
;;   (fold-right (lambda (x y) <??>) nil sequence))
;; (define (reverse sequence)
;;   (fold-left (lambda (x y) <??>) nil sequence))

;; from 1.2.6:
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))
;; ==

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; before:
;; (accumulate append
;;             nil
;;             (map (lambda (i)
;;                    (map (lambda (j) (list i j))
;;                         (enumerate-interval 1 (- i 1))))
;;                  (enumerate-interval 1 n)))

;; which could be written as:
(define (ord-pair-distinct-pos n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Exercise 2.40.  Define a procedure unique-pairs that, given an
;; integer n, generates the sequence of pairs (i,j) with 1< j< i<
;; n. Use unique-pairs to simplify the definition of prime-sum-pairs
;; given above.

