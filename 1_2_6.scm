(use numbers)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; Exercise 1.21.  Use the smallest-divisor procedure to find the smallest
;; divisor of each of the following numbers: 199, 1999, 19999.

;; 12> (smallest-divisor 199)
;; 199
;; #;13> (smallest-divisor 1999)
;; 1999
;; #;14> (smallest-divisor 19999)
;; 7

;; Heh, funny.


;; Exercise 1.22.  Most Lisp implementations include a primitive called
;; runtime that returns an integer that specifies the amount of time the
;; system has been running (measured, for example, in microseconds).
;; The following timed-prime-test procedure, when called with an integer n,
;; prints n and checks to see if n is prime. If n is prime, the procedure
;; prints three asterisks followed by the amount of time used in performing
;; the test.

(define (runtime) (inexact->exact (current-milliseconds)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))


;; Using this procedure, write a procedure search-for-primes that checks
;; the primality of consecutive odd integers in a specified range. Use your
;; procedure to find the three smallest primes larger than 1000 ; larger
;; than 10,000; larger than 100,000; larger than 1,000,000. Note the time
;; needed to test each prime. Since the testing algorithm has order of growth
;; of (n), you should expect that testing for primes around 10,000 should
;; take about 10 times as long as testing for primes around 1000. Do your
;; timing data bear this out?
;; -> it's a little less than (sqrt 10) even.
;;How well do the data for 100,000 and 1,000,000
;; support the n prediction?
;; -> Not well, since 100000 was 0.004s CPU time and 1000000 was 0.005s CPU time.
;;Is your result compatible with the notion that
;; programs on your machine run in time proportional to the number of steps
;; required for the computation? 

(define (search-for-primes start count)
  (if (= count 0)
      #t
      (if (prime? start)
          (begin
            (display start)
            (newline)
            (search-for-primes (+ start 1) (- count 1)))
          (search-for-primes (+ start 1) count))))

(define (timed-search-for-primes start count)
  (time (search-for-primes start count)))


