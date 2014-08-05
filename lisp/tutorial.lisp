; HSC3-LISP

; AMERICAN PRIMITIVE, VOL. 2
; SET!, λ, MACRO, IF, QUOTE

; EMACS LISP

; (setq rsc3-interpreter (list "hsc3-lisp"))

; NIL LISP

nil ; NIL
(null? nil) ; #t

; EQ LISP

(equal? 'a 'a) ; #t
(equal? "b" "b") ; #t
(= 5 5) ; #t

; ORD LISP

(< 0 1) ; #t
(> 0 1) ; #f
(min 1 2) ; 1
(max 1 2) ; 2
(compare 1 2) ; 'lt
(compare 2 1) ; 'gt
(compare 1 1) ; 'eq

; FUNCTION LISP

; Functions and procedures are strictly of the form α -> β.

+ ; FUN
(+ 1) ; FUN
((+ 1) 2) ; 3

; A notational extension is allowed

(+ 1 2) ; 3

; And another, for THUNKS

(lambda () 1) ; (λ _ 1)
((λ _ 1)) ; 1
(newline) ; \n

; There is no VARARG

(+ 1 2 3) ; ERROR
(sum '(1 2 3)) ; 6

(map (compose (+ 1) (* 2)) (list 1 2 3)) ; (3 5 7)
(map (compose (/ 2) (+ 3)) (list 1 2 3)) ; (1/2 2/5 1/3)
(map (const 3) (list 1 2 3)) ; (3 3 3)
(cons (- 1 2) ((flip -) 1 2)) ; (cons -1 1)
(id 1) ; 1

(procedure? +)

; CHURCH LISP

((λ n (* n n)) 3) ; 9

(lambda-rw '(lambda () x)) ; (λ _ x)
(lambda-rw '(lambda (x) x)) ; (λ x x)
(lambda-rw '(lambda (x y) (cons x y))) ; (λ x (λ y (cons x y)))
(lambda-rw '(lambda (x y z) (list x y z))) ; (λ x (λ y (λ z (list x y z))))

((lambda (n) (* n n)) 3) ; 9
((lambda (x y z) (+ x (+ y ((lambda (n) (* n n)) z)))) 1 2 3) ; 12

; MUTATING LISP

(set! a nil) ; NIL
a ; NIL
(set! a 5) ; NIL
a ; 5

(set! b (λ _ a)) ; NIL
(b) ; 5
(set! a 4) ; NIL
(b) ; 4

; SEQUENTIAL LISP

(begin-rw '(begin)) ; NIL
(begin-rw '(begin (display 1))) ; ((λ _ (display 1)) nil)
(begin-rw '(begin (display 1) (display 2))) ; ((λ _ (display 2)) ((λ _ (display 1)) nil))

(begin (display 1) (display 2) (display 3))
((λ _ (display 3)) ((λ _ (display 2)) ((λ _ (display 1)) nil)))

((λ x (begin (display x) (set! x 5) (display x))) 0) ; 05

; DEFINING LISP

; DEFINE is an alias for set!
(define-rw '(define one 1)) ; (set! one 1)

(define one 1) ; NIL
one ; 1

(define sq (λ n (* n n))) ; NIL
(sq 5) ; 25

(define sum-sq (lambda (p q) (+ (sq p) (sq q)))) ; NIL
(sum-sq 7 9) ; 130

not-defined ; ERROR
((lambda (_) (define not-defined 1)) nil) ; NIL
not-defined ; 1

; BINDING LISP

(let-rw '(let () 1)) ; 1
(let-rw '(let ((a 5)) (+ a 1))) ; ((λ a (+ a 1)) 5)
(let-rw '(let ((a 5) (b 6)) (+ a b))) ; ((λ a ((λ b (+ a b)) 6)) 5)

(let ((a 5)) a) ; 5
(let ((a 5) (b 6)) (cons a b)) ; (cons 5 6)
(let ((a 5) (b (+ 2 3))) (* a b)) ; 25
(let ((a 5) (b (+ a 3))) (* a b)) ; 40

; CONS LISP

(define c (cons 1 2)) ; NIL
(car c) ; 1
(cdr c) ; 2
(pair? c) ; #t
(list? c) ; #f
(null? c) ; #f
(null? '()) ; #t

; LIST LISP

(list-rw '(list)) ; NIL
(list-rw '(list 1 2 3)) ; (cons 1 (cons 2 (cons 3 nil)))
(list 1 2 3) ; (1 2 3)
list ; MACRO

(define l (cons 1 (cons 2 (cons 3 '())))) ; NIL
(null? l) ; #f
(pair? l) ; #t
(list? l) ; #t
(length l) ; 3
(append l l) ; (1 2 3 1 2 3)

(append '(1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)
(foldl + 0 (list 1 2 3)) ; 6
(foldr cons '() (list 1 2 3)) ; (1 2 3)
(list 1 2 3) ; (1 2 3)
(list 1 2 ((lambda (_) (cons 3 4)) nil) 5) ; (1 2 (cons 3 4) 5)
(map (+ 1) (list 1 2 3)) ; (2 3 4)
(maximum '(1 3 5 4 2 0)) ; 5
(minimum '(1 3 5 4 2 0)) ; 5
nil ; '()
(nub '(1 2 2 3 3 3)) ; (1 2 3)
(nub-by (on equal? car) '((0 1) (0 2) (1 2))) ; ((0 1) (1 2))
(not-elem 1 '(0 2 4))
(reverse (list 1 2 3)) ; (3 2 1)

; TREE LISP

(flatten '(1 2 (3 4 (5)) 6)) ; (1 2 3 4 5 6)
(levels '(1 2 (3 4 (5)) 6)) ; ((1 2 6) (3 4) (5))

; PRELUDE LISP

(enum-from-difference-to > 0 1 9) ; (0 1 2 3 4 5 6 7 8 9)
(enum-from-difference-to < 0 -3 -9) ; (0 -3 -6 -9)
(enum-from-then-to 0 1 9) ; (0 1 2 3 4 5 6 7 8 9)
(enum-from-then-to 0 -3 -9) ; (0 -3 -6 -9)
(enum-from-to 0 9) ; (0 1 2 3 4 5 6 7 8 9)
(map signum '(-3 0 3)) ; (-1 0 1)
(succ 1) ; 2
(pred 2) ; 1

; LOGICAL LISP

(if #t 1 2) ; 1
(if #f 1 2) ; 2

(define t #t) ; NIL
(if t 1 2) ; 1

(not #t) ; #f
(not #f) ; #t

(and-rw '(and p q)) ; (if p q 0)
(and #t #t) ; #t
(and #t #f) ; #f
(and #f #t) ; #f
(and #f #f) ; #f

(or-rw '(or p q)) ; (if p #t q)
(or #t #t) ; #t
(or #t #f) ; #t
(or #f #t) ; #t
(or #f #f) ; #f

(cond-rw '(cond)) ; NIL
(cond-rw '(cond (a b))) ; (if a b nil)
(cond-rw '(cond (a b) (c d))) ; (if a b (if c d nil))
(cond-rw '(cond (a b) (c d) (else e))) ; (if a b (if c d e))
(cond-rw '(cond ((> x y) 'gt) ((< x y) 'lt) (else 'eq)))

(when-rw '(when a b)) ; (if a b NIL)
(when #t (display "TRUE")) ; "TRUE"
(when #f (display "FALSE")) ; NIL

(when ((lambda (_) #t) nil) (display "TRUE")) ; "TRUE"
(when ((lambda (_) #f) nil) (display "FALSE")) ; NIL

; QUOTING LISP

(quote (+ 1 2)) ; (+ 1 2)
(eval 1) ; 1
(eval (eval 1)) ; 1
(eval (quote (+ 1 2))) ; 3

(1) ; ERROR

; MATHEMATICAL LISP

; Binary operator UGens are optimising.

(add 1 2) ; 3

; Symbolic aliases are given.

(+ 1 2) ; 3

; Constants are numbers.

(number? 1) ; #t
(number? 'one) ; #f
(number? (sin-osc kr 5 0)) ; #f

; TEMPORAL LISP

(begin (display "before\n") (pause-thread 1) (display "after\n"))

(utcr)

(let ((t (utcr)))
  (begin
    (display "before\n")
    (pause-thread-until (+ t 1))
    (display "after\n")))

(define random-sine (mul (sin-osc ar (rand 220 440) 9) 0.01))
(dt-rescheduler (lambda (t) (begin (hear random-sine) 1)) (utcr))

; IO LISP

(display 1) ; 1
(display (+ 1 2)) ; 3
(begin (display 1) (display 2)) ; 12
(define three (begin (display 1) (display 2) 3)) ; 12 NIL
three ; 3

; STRING LISP

"string" ; "string"
(string? "string") ; #t

; LOADING LISP

(load "/home/rohan/sw/hsc3-forth/lisp/stdlib.lisp")

; FLOATING LISP

(map sin (enum-from-then-to 0 0.05 pi))

; SICP

(define square (lambda (n) (* n n))) ; NIL

(define f
  (lambda (x y)
    ((lambda (a b) (+ (+ (* x (square a)) (* y b)) (* a b)))
     (+ 1 (* x y))
     (- 1 y))))

(f 7 9) ; 28088

; UGEN

(stop nil)
(draw (* (sin-osc ar 440 0) 0.1))
(draw (* (sin-osc ar (mouse-x kr 440 880 0 0.1) 0) 0.1))
(draw (* (hpz1 (white-noise ar)) 0.1))
(sc3-status nil)
(play (* (sin-osc ar 440 0) 0.1))

; UID

(set! uid 0) ; NIL
(map incr-uid '(1 1 1)) ; (1 2 3)
