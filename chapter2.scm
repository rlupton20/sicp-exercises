;;; Exercise 2.1
(define (better-make-rat n d)
  (let*
      ((g (gcd n d))
       (sign (lambda (x) (if (< x 0) -1 1)))
       (s (sign (* n d))))
    (cons (* s (/ (abs n) g))
          (/ (abs d) g))))

;;; Exercise 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment s e)
  (cons s e))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (average a b)
    (/ (+ a b) 2))

  (make-point
   (average (x-point (start-segment s)) (x-point (end-segment s)))
   (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define (test-midpoint-segment)
  (let ((s (make-segment (make-point -1 -1)
                         (make-point 1 1))))
    (midpoint-segment s)))

;;; Exercise 2.3
(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left r)
  (car r))

(define (bottom-right r)
  (cdr r))

(define (area r)
  (abs (* (- (x-point (top-left r)) (x-point (bottom-right r)))
          (- (y-point (top-left r)) (y-point (bottom-right r))))))

(define (perimeter r)
  (* 2 (+ (abs (- (x-point (top-left r)) (x-point (bottom-right r))))
          (abs (- (y-point (top-left r)) (y-point (bottom-right r)))))))

;; An alternative representation
(define (alt-make-rect top-left bottom-right)
  ;; Store the top-left as well as x and y deltas from this point
  (cons top-left
        (cons (- (x-point bottom-right) (x-point top-left))
              (- (y-point bottom-right) (y-point top-left)))))

(define (alt-top-left r)
  (car r))

(define (alt-bottom-right r)
  (make-point (+ (x-point (car r)) (car (cdr r)))
              (+ (y-point (car r)) (cdr (cdr r)))))

;;; Exercise 2.4
(define (alt-cons x y)
  (lambda (m) (m x y)))

(define (alt-car z)
  (z (lambda (p q) p)))

(define (alt-cdr z)
  (z (lambda (p q) q)))

;;; Exercise 2.5

;; That this works follows from unique prime factorisation
(define (nat-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (nat-component c k)
  "Helper for figuring out the prime power in a factorisation."
  (define (divides? d k)
    (= 0 (remainder k d)))

  (define (worker rem cur)
    (if (divides? k rem)
        (worker (quotient rem k) (+ cur 1))
        cur))

  (worker c 0))

(define (nat-car c)
  (nat-component c 2))

(define (nat-cdr c)
  (nat-component c 3))


;;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define (reduce-church k)
  "Utility for reducing Church numerals to normal integers."
  ((k (lambda (x) (+ 1 x))) 0))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))


;;; Exercise 2.7

;; First let's define the pieces in the question
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
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

;; Now for the exercise
(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

;;; Exercise 2.8
;; Note that the negation of an interval (lower, upper) is the interval (- upper, - lower)
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

;;; Exercise 2.9

;; For addition, observe that width (l, u) = 1/2 (u -l), and since
;; (u1 + u2) - (l1 + l2) = (u1 - l1) + (u2 -l2),
;; width (i1 + i2) = width i1 + width i2
;;
;; For subtraction, it suffices to argue that width (-u, -l) = width (l, u),
;; but this is clear since (-l - (-u)) = u - l.
;;
;; To provide examples to demonstrate that widths of multiplication and division are not
;; functions of the widths of the arguments, note we just need to provide for each,
;; two sets of arguments, with the same width pairs, with different composite widths.
;; We do this with examples in scheme.
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (assert c)
  "Simple assert function"
  (if (not c)
      (error "Assertion failed")))

(define (counter-examples)
  (let
      ((i1 (make-interval 1 2))
       (i2 (make-interval 2 3))
       (i3 (make-interval 3 4)))
    (assert (= (width i1)
               (width i2)
               (width i3)))
    (assert (not (= (width (mul-interval i1 i2))
                    (width (mul-interval i1 i3)))))
    (assert (not (= (width (div-interval i1 i2))
                    (width (div-interval i1 i3)))))
    'ok))

;;; Exercise 2.10
(define (better-div-interval x y)
  (define (span-zero? i)
    (<= (lower-bound i) 0 (upper-bound i)))

  (if (span-zero? y)
      (error "Interval spans zero - BETTER-DIV-INTERVAL: " y)
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;;; Exercise 2.11
(define (mul-interval-alternative x y)

  (define (positive? n)
    (<= 0 n))

  (define (negative? n)
    (not (positive? n)))

  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound u)))
    ;; We condition on whether each of our intervals are
    ;; positive, span 0, or negative. Three states for two
    ;; intervals, entails 3 * 3 = 9 combinations.
    (cond
     ;; Both intervals positive
     ((and (positive? xl) (positive? yl))
      (make-interval (* xl yl) (* xu yu)))
     ;; One interval positive, one spanning 0
     ((and (positive? xl) (negative? yl) (positive? yu))
      (make-interval (* xu yl) (* xu yu)))
     ((and (negative? xl) (positive? xu) (positive? yl))
      (make-interval (* xl yu) (* xu yu)))
     ;; One interval positive, one negative
     ((and (positive? xl) (negative? yu))
      (make-interval (* xu yl) (* xl yu)))
     ((and (negative? xu) (positive? yl))
      (make-interval (* xl yu) (* xu yl)))
     ;; Both intervals negative
     ((and (negative? xu) (negative? yu))
      (make-interval (* xu yu) (* xl yl)))
     ;; One interval negative, the other spanning 0
     ((and (negative? xu) (negative? yl) (positive? yu))
      (make-interval (* xl yu) (* xl yl)))
     ((and ((negative? xl) (positive? xu) (negative? yu)))
      (make-interval (* xu yl) (* xl yl)))
     ;; Both intervals spanning 0
     ((and (negative? xl) (negative? yl) (positive? xu) (positive? yu))
      (make-interval (min (* xl yu) (* xu yl))
                     (max (* xl yl) (* xu yu)))))))

;;; Exercise 2.12
(define (make-interval-percent c p)
  (let
      ((w (/ (* c p) 100)))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (let
      ((w (/ (- (upper-bound i) (lower-bound i)) 2)))
    (* (/ w (center i)) 100)))

;; The selector center is given
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;;; Exercise 2.13
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let
      ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (ex-2.14)
  (let
      ((a (make-interval 9.9 10.1))
       (b (make-interval 4.9 5.1))
       (center-and-percent (lambda (i) (cons
                                        (center i)
                                        (percent i)))))
    (display "Demonstrate the differences in par")
    (newline)
    (display (center-and-percent (par1 a a)))
    (newline)
    (display (center-and-percent (par2 a a)))
    (newline)
    (newline)
    (display "a/a and a/b")
    (newline)
    (display (center-and-percent (div-interval a a)))
    (newline)
    (display (center-and-percent (div-interval a b)))
    (newline)))

;;; Exercise 2.15
;; The key observation here is that by mentioning a variable twice, you double
;; count the error, which therefore increases the overall error (no operation
;; will decrease overall error). Therfore Eva is right.

;;; Exercise 2.16 TODO

;;; Exercise 2.17
(define (last-pair ls)
  (if (null? (cdr ls))
      ls
      (last-pair (cdr ls))))

;;; Exercise 2.18
(define (reverse ls)
  (define nil '())

  (define (reverse-iter rest reversed)
    (if (null? rest)
        reversed
        (reverse-iter (cdr rest) (cons (car rest) reversed))))

  (reverse-iter ls nil))

;;; Exercise 2.19
(define (cc amount coin-values)

  (define first-denomination car)
  (define except-first-denomination cdr)
  (define no-more? null?)

  (cond
   ((= amount 0) 1)
   ((or (< amount 0) (no-more? coin-values)) 0)
   (else (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

;;; Exercise 2.20
(define (same-parity p . r)
  (define (parity n)
    (remainder n 2))

  (define (filtered l)
    (cond
     ((null? l) l)
     ((= (parity p) (parity (car l))) (cons (car l) (filtered (cdr l))))
     (else (filtered (cdr l)))))

  (cons p (filtered r)))

;;; Exercise 2.21
;;; Let's define nil, since not all schemes seem to have it
(define nil '())

(define (square-list-direct items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;;; Exercise 2.23
(define (for-each proc items)

  (define (step items)
    ;; We could just use progn in the if block
    ;; but instead let's define an auxilliary function
    (proc (car items))
    (for-each proc (cdr items)))

  (if (null? items)
      '()
      (step items)))

;;; Exercise 2.25
(define (ex-2.25)
  (display
   (car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))))
  (display
   (car (car '((7)))))
  (display
   (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))))

;;; Exercise 2.27
(define (deep-reverse items)
  (define (iter items reversed)
    (cond
     ((null? items) reversed)
     ((pair? (car items)) (iter (cdr items)
                                (cons (deep-reverse (car items)) reversed)))
     (else (iter (cdr items) (cons (car items) reversed)))))

  (iter items nil))

;;; Exercise 2.28
(define (fringe items)
  (cond
   ((null? items) nil)
   ((pair? (car items)) (append (fringe (car items)) (fringe (cdr items))))
   (else (cons (car items) (fringe (cdr items))))))

;;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a)
(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

;; b)
(define (total-weight mobile)
  (define (branch-weight b)
    (if (pair? (branch-structure b))
        (total-weight (branch-structure b))
        (branch-structure b)))

  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; c)
(define (balanced? mobile)
  (define (branch-balanced? b)
    (if (pair? (branch-structure b))
        (balanced? (branch-structure b))
        #t))

  (define (branch-weight b)
    (if (pair? (branch-structure b))
        (total-weight (branch-structure b))
        (branch-structure b)))

  (define (branch-torque b)
    (* (branch-length b)
       (branch-weight b)))

  (and (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

;; d) If using cons in the constructor instead of list, we only need to change the selectors
;; in order for the programs in b) and c) to continue to work. Notice, however, that we
;; use pair? to scrutinise the structure of a branch - to be fully abstract, we should provide
;; a predicate to decide if a branch hangs a single weight, or another mobile.


;;; Exercise 2.30
(define (square-tree-direct tree)
  (cond
   ((null? tree) nil)
   ((not (pair? tree)) (square tree))
   (else (cons (square-tree-direct (car tree)) (square-tree-direct (cdr tree))))))

(define (square-tree-higher tree)
  (map (lambda (t)
         (if (pair? t)
             (square-tree-higher t)
             (square t)))
       tree))

;;; Exercise 2.31
(define (tree-map proc tree)
  (cond
   ((null? tree) nil)
   ((not (pair? tree)) (proc tree))
   (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))

;;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (r) (cons (car s) r)) rest)))))

;; This works because for a set (x . ys), the subsets are all subsets of ys,
;; and all subsets of ys with x added.