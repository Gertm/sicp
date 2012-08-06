;; The process that a procedure generates is of course dependent on the rules used by
;; the interpreter. As an example, consider the iterative gcd procedure given above.
;; Suppose we were to interpret this procedure using normal-order evaluation, as discussed
;; in section 1.1.5. (The normal-order-evaluation rule for if is described in exercise 1.5.)
;; Using the substitution method (for normal order), illustrate the process generated in
;; evaluating (gcd 206 40) and indicate the remainder operations that are actually performed.
;; How many remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)?
;; In the applicative-order evaluation?

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; normal-order evaluation: "fully expand and then reduce"

(gcd 206 40)
 |
 V
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

;; the if's will get evaluated here.
;; So it'll do all of those, but nothing else will. Remainder will be called a lot more than in app. order.


;; applicative-order evaluation: "evaluate the arguments and then apply"

(gcd 206 40)
 |
 V
(gcd 40 (remainder 206 40))
 |
 V
(gcd 40 6)
 |
 V
(gcd 6 (remainder 40 6))
 |
 V
(gcd 6 4)
 |
 V
(gcd 4 (remainder 6 4))
 |
 V
(gcd 4 2) -> (gcd 2 0) -> 2
