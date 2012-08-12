(use numbers)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define (h a b n)
    (/ (- b a) n))
  (* (/ h 3) (sum f a )))
