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

;;; Exercise 2.33
;; First let's define accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-by-accumulate p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-by-accumulate seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-by-accumulate sequence)
  (accumulate (lambda (_ y) (+ 1 y)) 0 sequence))

;;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;; Exercise 2.35
;; Let's use enumerate tree
(define (enumerate-tree t)
  (cond
   ((null? t) nil)
   ((not (pair? t)) (list t))
   (else (append (enumerate-tree (car t))
                 (enumerate-tree (cdr t))))))

(define (count-leaves-by-accumulate t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col) (dot-product row col)) cols))
         m)))

;;; Exercise 2.38
(define (fold-right op init seq)
  (cond
   ((null? seq) init)
   (else (op (car seq) (fold-right op init (cdr seq))))))

(define (fold-left op init seq)
  (define (iter result sequence)
    (if (null? sequence)
        result
        (iter (op result (car sequence)) (cdr sequence))))
  (iter init seq))

;; Want (op a b) = (op b a) and op to be associative to get fold-left and fold-right
;; to agree

;;; Exercise 2.39
(define (reverse-fold-right s)
  (fold-right (lambda (x y) (append y (list x))) nil s))

(define (reverse-fold-left s)
  (fold-left (lambda (x y) (cons y x)) nil s))

;;; Exercise 2.40
(define (enumerate-interval low high)
  (define (iter cur acc)
    (if (< cur low)
        acc
        (iter (- cur 1) (cons cur acc))))
  (iter high nil))

(define (flatmap p seq)
  (accumulate append nil (map p seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (k) (list i k))(enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (define (divides? n m)
    (= 0 (remainder n m)))

  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond
     ((< n (square test-divisor)) n)
     ((divides? n test-divisor) test-divisor)
     (else (find-divisor n (+ test-divisor 1)))))

  (define (prime? n)
    (and (not (= n 1)) (= n (smallest-divisor n))))

  (define (prime-sum? p)
    (prime? (+ (car p) (cadr p))))

  (define (pair-sum p)
    (list (+ (car p) (cadr p)) (car p) (cadr p)))

  (map pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;; Exercise 2.41
(define (sum-distinct-triples n s)
  (define (sum-pair p)
    "The sum of a pair p."
    (+ (car p) (cadr p)))

  (define (k p)
    "Given an i and j, compute a k"
    (- s (sum-pair p)))

  (define (permissible? triple)
    "Does our triple k, i, j meet the constraints?"
    (and (< (car triple) n)
         (< (cadr triple) (car triple))))

  (define (make-triple p)
    (cons (k p) p))

  (filter permissible?
          (map make-triple
               (unique-pairs n))))

;;; Exercise 2.42
(define (queens board-size)
  (define empty-board nil)

  (define (make-position r c)
    (cons r c))
  (define (row pos)
    (car pos))
  (define (column pos)
    (cdr pos))

  (define (adjoin-position r k rest-of-queens)
    (cons (make-position r k) rest-of-queens))

  (define (checks? pos1 pos2)
    (or (= (row pos1) (row pos2))
        (= (- (row pos1) (row pos2))
           (- (column pos1) (column pos2)))
        (= (+ (row pos1) (column pos1))
           (+ (row pos2) (column pos2)))))

  (define (safe? k positions)
    (let ((pos (car (filter (lambda (pos) (= k (column pos))) positions)))
          (rest (filter (lambda (pos) (not (= k (column pos)))) positions)))
      (null? (filter (lambda (other) (checks? pos other)) rest))))

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

;;; Exercise 2.43

;; The problem with Louis' solution is how many times he calls (queen-cols)
;; For each call to (queen-col k), Louis calls (queen-cols (- k 1)) board-size many times,
;; as opposed to once for the presented solution. Therefore, if the presented solution takes
;; time T, Louis' solution is likely to take around board-size * T time to complete, so 8 times
;; longer than the presented solution on an 8x8 board.


;;; Exercise 2.44
(define (up-split painter n)
  "Not really testable..."
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;; Exercise 2.45
(define (split bigger-to-smaller smaller-to-smaller)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (bigger-to-smaller painter (smaller-to-smaller smaller smaller)))))
  splitter)

;;; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
             (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;; Exercise 2.47
(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-list frame)
  (car frame))

(define (edge1-frame-list frame)
  (cadr frame))

(define (edge2-frame-list frame)
  (caddr frame))

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-cons frame)
  (car frame))

(define (edge1-frame-cons frame)
  (car (cdr frame)))

(define (edge2-frame-cons frame)
  (cdr (cdr frame)))

;;; Exercise 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;;; Exercise 2.49
(define (segments->painter segments-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segments-list)))

;; a)
(define outline
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 1 1)))))

;; b)
(define draw-x
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 0 1) (make-vect 1 0)))))

;; c)
(define draw-diamond
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))

;; d)
(define wave
  (segments->painter
   (list
    ;; Crotch
    (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
    (make-segment (make-vect 0.6 0) (make-vect 0.5 0.3))
    ;; Left-bottom
    (make-segment (make-vect 0.25 0) (make-vect 0.35 0.5))
    (make-segment (make-vect 0.35 0.5) (make-vect 0.25 0.6))
    (make-segment (make-vect 0.25 0.6) (make-vect 0.15 0.4))
    (make-segment (make-vect 0.15 0.4) (make-vect 0 0.6))
    ;; Left-top
    (make-segment (make-vect 0 0.8) (make-vect 0.15 0.6))
    (make-segment (make-vect 0.15 0.6) (make-vect 0.25 0.7))
    (make-segment (make-vect 0.25 0.7) (make-vect 0.4 0.7))
    (make-segment (make-vect 0.4 0.7) (make-vect 0.25 0.85))
    (make-segment (make-vect 0.25 0.85) (make-vect 0.4 1))
    ;; Right-bottom
    (make-segment (make-vect 0.75 0) (make-vect 0.6 0.4))
    (make-segment (make-vect 0.6 0.4) (make-vect 1 0.2))
    ;; Right-top
    (make-segment (make-vect 1 0.3) (make-vect 0.8 0.7))
    (make-segment (make-vect 0.8 0.7) (make-vect 0.6 0.7))
    (make-segment (make-vect 0.6 0.7) (make-vect 0.75 0.85))
    (make-segment (make-vect 0.75 0.85) (make-vect 0.6 1)))))

;;; Exercise 2.50
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-cc-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-cc-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;;; Exercise 2.51
(define (below bottom top)
  (let ((split-point (make-vect 0.0 0.5)))
    (let
        ((paint-bottom (transform-painter bottom
                                          (make-vect 0.0 0.0)
                                          (make-vect 1.0 0.0)
                                          split-point))
         (paint-top (transform-painter top
                                       split-point
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-using-beside bottom top)
  (flip-horiz
   (rotate-cc-270
    (beside bottom top))))

;;; Exercise 2.52

;; a)
(define wave-mod
  (segments->painter
   (list
    ;; Crotch
    (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
    (make-segment (make-vect 0.6 0) (make-vect 0.5 0.3))
    ;; Left-bottom
    (make-segment (make-vect 0.25 0) (make-vect 0.35 0.5))
    (make-segment (make-vect 0.35 0.5) (make-vect 0.25 0.6))
    (make-segment (make-vect 0.25 0.6) (make-vect 0.15 0.4))
    (make-segment (make-vect 0.15 0.4) (make-vect 0 0.6))
    ;; Left-top
    (make-segment (make-vect 0 0.8) (make-vect 0.15 0.6))
    (make-segment (make-vect 0.15 0.6) (make-vect 0.25 0.7))
    (make-segment (make-vect 0.25 0.7) (make-vect 0.4 0.7))
    (make-segment (make-vect 0.4 0.7) (make-vect 0.25 0.85))
    (make-segment (make-vect 0.25 0.85) (make-vect 0.4 1))
    ;; Right-bottom
    (make-segment (make-vect 0.75 0) (make-vect 0.6 0.4))
    (make-segment (make-vect 0.6 0.4) (make-vect 1 0.2))
    ;; Right-top
    (make-segment (make-vect 1 0.3) (make-vect 0.8 0.7))
    (make-segment (make-vect 0.8 0.7) (make-vect 0.6 0.7))
    (make-segment (make-vect 0.6 0.7) (make-vect 0.75 0.85))
    (make-segment (make-vect 0.75 0.85) (make-vect 0.6 1))
    ;; (Straight) smile
    (make-segment (make-vect 0.4 0.8) (make-vect 0.6 0.8)))))

;; b)
(define (corner-split-mod painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split-mod painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

;; c)
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit-mod painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))


;;; Exercise 2.54
(define (my-equal? x y)
  (cond
   ((and (pair? x) (pair? y))
    (and (my-equal? (car x) (car y))
         (my-equal? (cdr x) (cdr y))))
   (else (eq? x y))))

;;; Exercise 2.55
;; (car ''abracadabra) is the same as (car '(quote abracadabra)), from which it is
;; clear that (car ''abracadrabra) is equivalent to the symbol quote

;;; Exercise 2.56

;; First let's duplicate the functions we need
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2)) (* m1 m2))
   (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

;; Now we can extend deriv
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation
            (base exp)
            (make-sum (exponent exp) -1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-exponentiation b e)
  (cond
   ((=number? e 0) 1)
   ((=number? e 1) b)
   ((and (number? b) (number? e) (expt b e)))
   (else (list '** b e))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

;;; Exercise 2.57
;; We define new constructors and selectors for sum and product
(define (make-sum . as)
  (let ((exps           ; Remove all 0s
         (filter (lambda (exp) (not (=number? exp 0))) as)))
    (cond
     ((null? exps) 0)
     ((not (null? (cdr exps))) (cons '+ exps))
     (else (car exps)))))

(define (addend s)
  (cadr s))

(define (augend s)
  (apply make-sum (cddr s)))

(define (make-product . ms)
  (define (find p xs)
    (cond
     ((null? xs) #f)
     ((p (car xs)) (car xs))
     (else (find p (cdr xs)))))

  (if (find (lambda (exp) (=number? exp 0)) ms)
      0                 ; We have a zero
      (let ((exps       ; Remove all 1s
             (filter (lambda (exp) (not (=number? exp 1))) ms)))
        (cond
         ((null? exps) 1)
         ((not (null? (cdr exps))) (cons '* exps))
         (else (car exps))))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (apply make-product (cddr p)))


;;; Exercise 2.58
;; a)
(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (number? a1) (number? a2)) (+ a1 a2))
   (else (list a1 '+ a2))))

(define (sum? s)
  (and (pair? s) (eq? '+ (cadr s))))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2)) (* m1 m2))
   (else (list m1 '* m2))))

(define (product? p)
  (and (pair? p) (eq? '* (cadr p))))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

;;b)
(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (number? a1) (number? a2)) (+ a1 a2))
   (else (list a1 '+ a2))))

(define (sum? s)
  (define (finds-sum? t)
    (cond
     ((null? t) #f)
     ((eq? '+ (car t)) #t)
     (else (finds-sum? (cdr t)))))

  (and (pair? s) (finds-sum? s)))

(define (split-at sym s)
  "Split the expression s at the first occurrence of s and simplify"
  (define (strip-parens expr)
    "Remove extraneous parentheses"
    (if (and (pair? expr) (null? (cdr expr)))
        (strip-parens (car expr))
        expr))

  (define (iter rh t)
    (cond
     ((eq? sym (car t))
      (cons
       (strip-parens (reverse rh))
       (strip-parens (cdr t))))
     (else (iter
            (cons (car t) rh)
            (cdr t)))))

  (iter '() s))

(define (addend s)
  (car (split-at '+ s)))

(define (augend s)
  (cdr (split-at '+ s)))

(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2)) (* m1 m2))
   ((and (product? m1) (product? m2)) (append m1 (cons '* m2)))
   ((product? m1) (append m1 (list '* m2)))
   ((product? m2) (cons m1 (cons '* m2)))
   (else (list m1 '* m2))))

(define (product? p)
  "We're a product if we have a highest level * and no lower precedence operators (+)"
  (define (finds-product? q)
    (cond
     ((null? q) #f)
     ((eq? '* (car q)) #t)
     (else (finds-product? (cdr q)))))

  (and (pair? p)
       (not (sum? p))
       (finds-product? p)))

(define (multiplier p)
  (car (split-at '* p)))

(define (multiplicand p)
  (cdr (split-at '* p)))

;;; Exercise 2.59
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((equal? (car set) x) #t)
   (else (element-of-set? x (cdr set)))))

(define (union-set s t)
  (cond
   ((null? s) t)
   ((null? t) s)
   ((element-of-set? (car s) t) (union-set (cdr s) t))
   (else (union-set (cdr s) (cons (car s) t)))))

;;; Exercise 2.60
;; element-of-set? remains the same in the non-deduplicated representation
;; adjoin-set and intersection-set also remains the same - let's implement it
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set1) set2) (cons (car set1)
                                            (intersection-set (cdr set1) set2)))
   (else (intersection-set (cdr set1) set2))))

;; union can be made more efficient - we can just append
(define (union-set-dup set1 set2)
  (append set1 set2))

;; Efficiency is comparable - unions are faster, being linear in the sum of the sizes
;; of the sets. Sets may be larger than their number of elements, however, since there
;; is no deduplication.
;;
;; If fast unions are a main concern, then the duplicated version of set may be
;; preferable to the deduplicated one.

;;; Exercise 2.61
(define (adjoin-set x set)
  (cond
   ((null? set) (list x))
   ((= x (car set)) set)
   ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))
   (else (cons x set))))

;;; Exercise 2.62
(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   (else
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond
       ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
       ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
       (else (cons x2 (union-set set1 (cdr set2)))))))))

;;; Exercise 2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
              (cons (entry tree)
                    (copy-to-list (right-branch tree) result-list)))))

  (copy-to-list tree '()))

;; a) Both of these procedures produce the same output for the same input tree
;; Figure 2.16 produces
;; - (1 3 5 7 9 11)

;; tree->list-2 cons-es each entry of the tree onto the result-list once, and
;; grows theta(n), where n is the number of entries in a (balanced) binary tree.
;; Balance of the tree is not required for this result.

;; tree-list-1 is slightly less efficient. tree->list-1 is called n times, once
;; for each entry in the tree, but the use of append means elements are cons-ed
;; onto a results list more than once.
;;
;;     a
;;    / \
;;   b   r
;;  / \
;; c   d
;;
;; Observe in the above c will be appended to (b d), then (c b d) will be appended
;; to (a r), so in the append c will be touched twice. In general, an entry will be
;; cons-ed proportionally to the log of it's left-branching-depth.
;;
;; Another way to look at this is that for a balanced tree, each first argument to
;; append has approximately half elements which have been cons-ed before. So for a
;; tree with n elements, approximately n/2 entries are cons-ed once, approximately
;; (n/4) cons-ed twice, etc. In sum this means approximately (n/2) * log n cons-es
;; happen, so the number of list cons-es grows as n*log(n).

;;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a) partial tree works by observing that for a balanced tree, roughly
;; half of the elements will end up in the left branch, and half in the
;; right (with one for the entry). It works by consuming enough elements
;; for the left subtree, and forming a tree (by calling itself recursively).
;; it then takes an element for the entry, and forms the right subtree with
;; the remaining elements, gluing together the pieces of the tree, and
;; returning that tree with the unconsumed elements.
;;
;; (list->tree '(1 3 5 7 9 11)) produces:
;;
;;           5
;;          / \
;;         1   \
;;          \   \
;;           3   9
;;              / \
;;             7  11

;; b) The order of growth of list-to-tree to convert a list of n elements
;; is theta(n). To see this, observe that we call partial-tree precisely
;; once for each entry in the tree.

;;; Exercise 2.65
;; We use list->tree and tree->list-2, both of which have theta(n) growth,
;; where n is the number of elements in the list/tree passed as input.

;; union-set will consist of converting the trees to lists, merging the two
;; lists, and then forming a tree.
;; intersection-set will consist of converting the trees to lists, and then
;; intersecting the two ordered lists.

(define (union-ordered-list list1 list2)
  (cond
   ((null? list1) list2)
   ((null? list2) list1)
   ((= (car list1) (car list2)) (cons (car list1)
                                      (union-ordered-list (cdr list1) (cdr list2))))
   ((< (car list1) (car list2)) (cons (car list1)
                                      (union-ordered-list (cdr list1) list2)))
   (else (cons (car list2) (union-ordered-list list1 (cdr list2))))))


(define (intersection-ordered-list list1 list2)
  (cond
   ((or (null? list1) (null? list2)) '())
   ((= (car list1) (car list2)) (cons (car list1)
                                      (intersection-ordered-list (cdr list1) (cdr list2))))
   ((< (car list1) (car list2)) (intersection-ordered-list (cdr list1) list2))
   (else (intersection-ordered-list list1 (cdr list2)))))

(define (union-set set1 set2)
  (list->tree
   (union-ordered-list (tree->list-2 set1)
                       (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree
   (intersection-ordered-list (tree->list-2 set1)
                              (tree->list-2 set2))))

;;; Exercise 2.66
(define (lookup given-key set-of-records)
  (cond
   ((null? set-of-records) #f)
   ((= given-key (key (entry set-of-records))) (entry set-of-records))
   ((< given-key (key (entry set-of-records))) (lookup given-key
                                                       (left-branch set-of-records)))
   (else (lookup given-key (right-branch set-of-records)))))

;; We can test this with a trivial key function and a set of records consisting of
;; just keys
(define (key record)
  record)
