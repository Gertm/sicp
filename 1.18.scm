;; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process
;; for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.

(use numbers)

(define (halve n)
  (/ n 2))

(define (double n)
  (* n 2))

(define (imult a b)
  (iter-mult a b 0))

(define (iter-mult a b acc)
  (if (= b 0)
      acc
      (cond ((even? b) (iter-mult (double a) (halve b) acc))
            (else (iter-mult a (- b 1) (+ acc a))))))

