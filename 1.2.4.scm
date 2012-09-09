(define (expt b n)
  (write 'i)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expti b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (write 'i)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
