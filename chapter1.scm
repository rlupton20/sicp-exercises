;;; Exercise 1.3

(define (sum-squares x y)
  "Return the sum of squares of x and y"
  (+ (* x x) (* y y)))


(define (ex-1-3 x y z)
  "Return the sum of the squares of the biggest two of x, y and z"
  (cond
   ((and (> x z) (> y z)) (sum-squares x y))
   ((and (> x y) (> z y)) (sum-squares x z))
   (else (sum-squares y z))))


;;; Exercise 1.7

;; The original version

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (bad-small-error)
  (sqrt (square 0.001)))

(define (bad-large-error)
  (sqrt (square 10000000000000100000)))

;; Let's try and improve on this by limiting the amount
;; the guess can improve

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(define (new-sqrt-iter guess x)
  (if (small-difference? guess (improve guess x))
      guess
      (new-sqrt-iter (improve guess x) x)))

(define (small-difference? a b)
  (< (abs (- a b)) 0.001))


;;; Exercise 1.11

(define (f-rec n)
  (cond
   ((< n 3) n)
   (else (+
          (f-rec (- n 1))
          (* 2 (f-rec (- n 2)))
          (* 3 (f-rec (- n 3)))))))

(define (f-iter n)
  (define (f-iter-worker acc a b count)
    (cond
     ((= count n) acc)
     (else (f-iter-worker
            (+ acc (* 2 a) (* 3 b))
            acc
            a
            (+ count 1)))))

  (cond
   ((< n 3) n)
   (else (f-iter-worker 2 1 0 2))))


;;; Exercise 1.12
(define (pascals-triangle row entry)
  "Recursively compute an entry from pascals triangle."

  (define (pascal-worker row entry)
    (cond
     ((or (< row 0) (< entry 0) (> entry row)) 0)
     ((= row 0) (= entry 0) 1)
     (else (+
            (pascal-worker (- row 1) entry)
            (pascal-worker (- row 1) (- entry 1))))))

  (define (filter-valid-res res)
    (cond
     ((= 0 res) '())
     (else res)))

  (filter-valid-res (pascal-worker row entry)))


;;; Exercise 1.16
(define (fast-expt a n)
  "Tail recursive exponentiation done in logarithmic (in n) time."
  (define (square x)
    (* x x))

  (define (halve x)
    (quotient x 2))

  (define (fast-expt-worker a n acc)
    (cond
     ((= n 0) acc)
     ((even? n) (fast-expt-worker (square a) (halve n) acc))
     (else (fast-expt-worker a (- n 1) (* acc a)))))

  (fast-expt-worker a n 1))


;;; Exercise 1.17
(define (fast-mul n m)
  "Multiple n by m using only addition and a halve and double operator"
  (define (double x)
    (* 2 x))

  (define (halve x)
    (quotient x 2))

  (define (even? x)
    (= x (double (halve x))))

  (define (do-fast-mul m)
    (cond
     ((= m 0) 0)
     ((even? m) (double (do-fast-mul (halve m))))
     (else (+ n (do-fast-mul (- m 1))))))

  (do-fast-mul m))

;;; Exercise 1.18
(define (linear-fast-mul n m)
  "Multiple n by m using only addition and a halve and double operator, by iteration."
  (define (double x)
    (* 2 x))

  (define (halve x)
    (quotient x 2))

  (define (even? x)
    (= x (double (halve x))))

  (define (do-fast-mul n m acc)
    (cond
     ((= m 0) acc)
     ((even? m) (do-fast-mul (double n) (halve m) acc))
     (else (do-fast-mul n (- m 1) (+ acc n)))))

  (do-fast-mul n m 0))

;;; Exercise 1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (cond
     ((= count 0) b)
     ((even? count) (fib-iter
                     a
                     b
                     (+ (* p p) (* q q))
                     (+ (* q q) (* 2 p q))
                     (/ count 2)))
     (else (fib-iter
            (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p
            q
            (- count 1)))))

  (fib-iter 1 0 0 1 n))

(define (fib-iter n)
  (define (fib-worker a b count)
    (cond
     ((= count 0) b)
     (else (fib-worker (+ a b) a (- count 1)))))

  (fib-worker 1 0 n))


;;; Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
   ((< n (square test-divisor)) n)
   ((divides? n test-divisor) test-divisor)
   (else (find-divisor n (+ test-divisor 1)))))

(define (divides? n m)
  (= 0 (remainder n m)))

(define (ex-1-21)
  (display (smallest-divisor 199))
  (newline)
  (display (smallest-divisor 1999))
  (newline)
  (display (smallest-divisor 19999))
  (newline))


;;; Exercise 1.22
(define (prime? n)
  (and (not (= n 1)) (= n (smallest-divisor n))))

(define (timed-prime-test n)
  (display n)
  (newline)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lower upper)
  (cond
   ((>= lower upper) '())
   ((even? lower) (search-for-primes (+ lower 1) upper))
   (else (begin
          (timed-prime-test lower)
          (newline)
          (search-for-primes (+ lower 2) upper)))))

(define (ex-1-22)
  "All the ranges where the first three primes can be found."
  (search-for-primes 1000 1020)
  (search-for-primes 10000 10038)
  (search-for-primes 100000 100044)
  (search-for-primes 1000000 1000038))

;;; Exercise 1.23
(define (next-divisor n)
  (cond
   ((= n 2) 3)
   (else (+ n 2))))


(define (smallest-divisor-alt n)
  (find-divisor n 2))

(define (find-divisor-alt n test-divisor)
  (cond
   ((< n (square test-divisor)) n)
   ((divides? n test-divisor) test-divisor)
   (else (find-divisor n (next-divisor test-divisor)))))

(define (prime-alt? n)
  (= n (smallest-divisor-alt n)))

;; TODO Finish

;;; Exercise 1.27
(define (carmichael? n)
  (define (expmod a n mod)
    (cond
     ((= n 0) 1)
     ((divides? n 2) (remainder
                      (square (expmod a (quotient n 2) mod))
                      mod))
     (else (remainder
            (* a (expmod a (- n 1) mod))
            mod))))

  (define (test-from a)
    (cond
     ((= a n) #t)
     ((= (expmod a n n) a) (test-from (+ a 1)))
     (else #f)))

  (and (not (prime? n)) (test-from 1)))

;;; Exercise 1.28
(define (miller-rabin-expmod a n mod)
  (define (miller-rabin-square a n mod)
    (let*
        ((x (miller-rabin-expmod-worker a (quotient n 2) mod))
         (y (remainder (square x) mod)))
      (cond
       ((and
         (= y 1)
         (not (= x 1))
         (not (= x (- mod 1)))) 0)
       (else y))))

  (define (miller-rabin-expmod-worker a n mod)
    (cond
     ((= n 0) 1)
     ((divides? n 2) (miller-rabin-square a n mod))
     (else (remainder
            (* a (miller-rabin-expmod-worker a (- n 1) mod))
            mod))))

  (let ((r (miller-rabin-expmod-worker a n mod)))
    (if (= r 0)
        'not-prime
        r)))

(define (miller-rabin-single-test? a n)
  "Does a^(n - 1) = 1 mod n, with no non-trivial square roots of 1?"
  (let ((r (miller-rabin-expmod a (- n 1) n)))
    (cond
     ((not (eq? r 1)) #f)
     ((eq? r 'not-prime) #f)
     (else #t))))

(define (miller-rabin-fast-prime? n num-tests)
  (define (miller-rabin-random-test n)
    (let ((a (+ (random (- n 1)) 1)))
      (miller-rabin-single-test? a n)))

  (cond
   ((= num-tests 0) #t)
   ((miller-rabin-random-test n) (miller-rabin-fast-prime? n (- num-tests 1)))
   (else #f)))

;;; Exercise 1.30
;; We do exercise 1.30 first so we have a sum procedure available
(define (sum term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ (term x) result))))
  (iter a 0))

;;; Exercise 1.29
;; First let's define the integral function for comparison
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpsons-rule f a b n)
  "Requires n to be even"
  (let ((h (/ (- b a) n)))

    (define (y k)
      (f (+ a (* k h))))

    (define (p k)
      (cond
       ((= k 0) 1)
       ((= k n) 1)
       ((divides? k 2) 2)
       (else 4)))

    (* (/ h 3.0)
       (sum (lambda (k) (* (p k) (y k)))
            0
            (lambda (k) (+ k 1))
            (+ n 1)))))

;; TODO Check

;;; Exercise 1.31
;; a)

(define (product term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* (term x) result))))
  (iter a 1))

(define (factorial-using-product n)
  (product
   (lambda (x) x)
   1
   (lambda (x) (+ x 1))
   n))

;; For approximating pi, note that we can use a pair to figure out
;; successive fractions:
;; Use the input (m, n) and define (next (m, n) (n + 1, m + 1))
;; This ought to yield a nice expression

;; But we want to use product
(define (pi-approx n)
  "Needs n > 2; pi / 4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7"

  (define (fracs-for n)
    (/ (* n n) (* (- n 1) (+ n 1))))

  (* 4.0 (/ 2 3) (product fracs-for 4 (lambda (x) (+ x 2)) (max n 3))))

;; Just for fun
(define (pi-approx-direct n)
  (define (next-pair p)
    (list (+ 1 (cadr p)) (+ 1 (car p))))
  (define (divide-pair p)
    (/ (car p) (cadr p)))

  (define (iter i pair result)
    (if (= i 0)
        result
        (iter (- i 1) (next-pair pair) (* result (divide-pair pair)))))

  (* 4.0 (iter n '(2 3) 1)))

;; b)

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a) (product-recursive term (next a) next b))))

;;; Exercise 1.32
;; a) Note that this is both recursive and iterative - it's possible to break
;; out an iterator but what's the point?
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate combiner
                  (combiner (term a) null-value)
                  term
                  (next a)
                  next
                  b)))

(define (sum-via-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (product-via-accumulate term a next b)
  (accumulate * 1 term a next b))

;; b)
;; A strict recursive version is possible, but it changes the semantics if
;; combiner isn't commutative
(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-recursive combiner
                                               null-value
                                               term
                                               (next a)
                                               next
                                               b))))

;; A pure iterator version, just because I can. This seems less concise than
;; the first solution, but provides the same semantics
(define (accumulate-iter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner (term x) result))))
  (iter a null-value))

;;; Exercise 1.33
(define (filtered-accumulate pred combiner null-value term a next b)
  (cond
   ((> a b) null-value)
   ((pred a) (filtered-accumulate pred
                                  combiner
                                  (combiner (term a) null-value)
                                  term
                                  (next a)
                                  next
                                  b))
   (else (filtered-accumulate pred
                              combiner
                              null-value
                              term
                              (next a)
                              next
                              b))))

;; a) Sum of squares of prime numbers in a to b
(define (sum-of-prime-squares a b)
  (filtered-accumulate prime? + 0 square a (lambda (x) (+ x 1)) b))

;; b) the product of all integers less than n, which are relatively prime to n
(define (product-of-n-coprime n)
  (filtered-accumulate (lambda (k) (= 1 (gcd k n)))
                       *
                       1
                       (lambda (x) x)
                       1
                       (lambda (x) (+ x 1))
                       (- n 1)))

;;; Exercise 1.35
;; First the fixed-point functions
(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v2 v1)) tolerance))
  (define (try guess)
    (let
        ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

;; The golden ratio is a fixed point of x |-> 1 + 1/x
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;;; Exercise 1.36
(define (fixed-point-trace f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v2 v1)) tolerance))
  (define (try guess)
    (let
        ((next (f guess)))
      (display "guess: ")
      (display next)
      (newline)
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(define (solution-basic)
  (fixed-point-trace (lambda (x) (/ (log 1000) (log x))) 1.1))

(define (solution-average-damping)
  (fixed-point-trace (lambda (x) (average x (/ (log 1000) (log x)))) 1.1))

;;; Exercise 1.37
;; a)
(define (cont-frac n d k)
  (define (worker r)
    (if (> r k)
        0
        (/ (n r) (+ (d r) (worker (+ r 1))))))
  (worker 1))

(define cont-frac-reciprocal-golden-ratio
  (cont-frac
   (lambda (x) 1.0)
   (lambda (x) 1.0)
   12))

;; b)
;; For the iterative process, start from the bottom and work up
(define (cont-frac-iter n d k)
  (define (worker level acc)
    (cond
     ((= level 0) acc)
     (else (worker (- level 1) (/ (n level) (+ (d level) acc))))))
  (worker k 0))

;;; Exercise 1.38
(define (e-approx k)
  (define (n x)
    1.0)
  (define (d x)
    (cond
     ((= (remainder x 3) 2) (* 2 (+ 1 (quotient x 3))))
     (else 1)))
  (+ 2 (cont-frac-iter n d k)))

;;; Exercise 1.39
;; TODO Check depth
(define (tan-cf x k)
  "Recursive procedure to generate tan x using continued fraction."
  (define (square x)
    (* x x))

  (define (b n)
    (+ (* 2 n) 1))

  (define (worker n)
    (cond
     ((= n k) (b k))
     (else (- (b n) (/ (square x) (worker (+ n 1)))))))

  (/ x (worker 0)))

;; And for completeness
(define (tan-cf-iter x k)
  "Iterative procedure to generate tan x using a continued fraction."
  (define (square x)
    (* x x))

  (define (b n)
    (+ (* 2 n) 1))

  (define (worker n acc)
    (if (< n 0)
        acc
        (worker (- n 1) (- (b n) (/ (square x) acc)))))

  (/ x (worker k (b k))))

;;; Exercise 1.40

(define (cubic a b c)
  (define (square x)
    (* x x))

  (define (cube x)
    (* x (square x)))

  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; TODO Actually find zeros

;;; Exercise 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ 1 x))

(define (ex-1.41)
  (((double (double double)) inc) 5))

;;; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (ex-1.42)
  (= ((compose square inc) 6) 49))

;;; Exercise 1.43
(define (repeated f n)
  (cond
   ((= n 1) f)
   ((> n 1) (lambda (x) (f ((repeated f (- n 1)) x))))
   (else f)))

(define (repeated-point-free f n)
  (cond
   ((< n 2) f)
   (else (compose f (repeated-point-free f (- n 1))))))

(define (ex-1.43)
  (and
   (= ((repeated square 2) 5) 625)
   (= ((repeated-point-free square 2) 5) 625)))

;;; Exercise 1.44
(define (smooth-dx dx)
  "Creates a smoothing function for smoothing over the range dx."
  (lambda (f)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
                   3))))

(define (smooth f)
  ((smooth-dx 0.001) f))

(define (n-fold-smooth n)
  (repeated smooth n))


;;; Exercise 1.45
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; Define a function to experiment with
(define (root-finder n k x)
  "Finds the nth root of x y, using average damping k times."
  (define (iter-fn y)
    (/ x (fast-expt y (- n 1))))

  (fixed-point ((repeated average-damp k) iter-fn) 1.0))

;; Seem to need an additional average damp at every power of 2
(define (nth-root n x)
  (define (log2 y)
    (/ (log y) (log 2)))

  (define (iter-fn y)
    (/ x (expt y (- n 1))))

  (let
      ((num-damps (ceiling (log2 n))))
    (fixed-point ((repeated average-damp num-damps) iter-fn) 1.0)))

;;; Exercise 1.46
(define (iterative-improve good-enough? next-guess)
  (define (iterator guess)
    (if (good-enough? guess)
        guess
        (iterator (next-guess guess))))
  iterator)

(define (sqrt-iterative-improve x)
  ((iterative-improve
    (lambda (y) (< (abs (- x (square y))) 0.0001))
    (lambda (y) (average y (/ x y))))
   1.0))

(define (fixed-point-iterative-improve f first-guess)
  ((iterative-improve
    (lambda (y) (< (abs (- y (f y))) tolerance))
    f)
   first-guess))

(define golden-ratio-iterative-improve
  (fixed-point-iterative-improve (lambda (x) (+ 1 (/ 1 x))) 1.0))
