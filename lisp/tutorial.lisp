; HSC3-LISP, AMERICAN PRIMITIVE, VOL. 2

; LAMBDA (a -> b), SET!, IF, BEGIN, QUOTE, MACRO

; EMACS LISP

; (setq rsc3-interpreter (list "hsc3-lisp"))

; THE VOID

void ; VOID
(void? void) ; #t

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

; One obvious notational extension is allowed, another is not.

(+ 1 2) ; 3
(+ 1 2 3) ; ERROR
(sum '(1 2 3)) ; 6

(map (compose (+ 1) (* 2)) (list 1 2 3)) ; (3 5 7)
(map (compose (/ 2) (+ 3)) (list 1 2 3)) ; (1/2 2/5 1/3)
(map (const 3) (list 1 2 3)) ; (3 3 3)
(cons (- 1 2) ((flip -) 1 2)) ; (cons -1 1)
(id 1) ; 1

; CHURCH LISP

((lambda (n) (* n n)) 3) ; 9
((lambda (x y z) (+ x (+ y ((lambda (n) (* n n)) z)))) 1 2 3) ; 12

; DEFINITE LISP

(define one 1) ; VOID
one ; 1

(define sq (lambda (n) (* n n))) ; VOID
(sq 5) ; 25

(define sum-sq (lambda (p q) (+ (sq p) (sq q)))) ; VOID
(sum-sq 7 9) ; 130

(define (lambda (_) (define undefined 1))) ; ERROR

; CONS LISP

(define c (cons 1 2)) ; VOID
(car c) ; 1
(cdr c) ; 2
(pair? c) ; #t
(list? c) ; #f
(null? c) ; #f
(null? '()) ; #t

; LIST LISP

(list-rw '(1 2 3)) ; (cons 1 (cons 2 (cons 3 nil)))
(list 1 2 3) ; (1 2 3)
list ; MACRO

(define l (cons 1 (cons 2 (cons 3 '())))) ; VOID
(null? l) ; #f
(pair? l) ; #t
(list? l) ; #t
(length l) ; 3
(append l l) ; (1 2 3 1 2 3)

(append '(1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)
(foldl + 0 (list 1 2 3)) ; 6
(foldr cons '() (list 1 2 3)) ; (1 2 3)
(list 1 2 3) ; (1 2 3)
(list 1 2 ((lambda (_) (cons 3 4)) void) 5) ; (1 2 (cons 3 4) 5)
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

(enum-from-then-to 0 1 9) ; (0 1 2 3 4 5 6 7 8 9)
(enum-from-to 0 9) ; (0 1 2 3 4 5 6 7 8 9)
(map signum '(-3 0 3)) ; (-1 0 1)
(succ 1) ; 2
(pred 2) ; 1

; LOGICAL LISP

(if #t 1 2) ; 1
(if #f 1 2) ; 2

(define t #t) ; VOID
(if t 1 2) ; 1

(not #t) ; #f
(not #f) ; #t

(and-rw '(p q)) ; (if p q 0)
(and #t #t) ; #t
(and #t #f) ; #f
(and #f #t) ; #f
(and #f #f) ; #f

(or-rw '(p q)) ; (if p #t q)
(or #t #t) ; #t
(or #t #f) ; #t
(or #f #t) ; #t
(or #f #f) ; #f

(cond-rw '((a b) (c d))) ; (if a b (if c d void))
(cond-rw '((a b) (c d) (else e))) ; (if a b (if c d e))

(when-rw '(a b)) ; (if a b VOID)
(when #t (display "TRUE")) ; "TRUE"
(when #f (display "FALSE")) ; VOID

(when ((lambda (_) #t) void) (display "TRUE")) ; "TRUE"
(when ((lambda (_) #f) void) (display "FALSE")) ; VOID

; MUTATING LISP

(define a '()) ; VOID
(set! a 5) ; VOID
a ; 5

(define a 5) ; VOID
(define b (lambda (_) a)) ; VOID
(b void) ; 5
(set! a 4)
(b void) ; 4

; BINDING LISP

(let ((a 5)) a) ; 5
(let ((a 5) (b 6)) (cons a b)) ; (cons 5 6)
(let ((a 5) (b (+ 2 3))) (* a b)) ; 25
(let ((a 5) (b (+ a 3))) (* a b)) ; 40

; QUOTING LISP

(quote (+ 1 2)) ; (+ 1 2)
(eval 1) ; 1
(eval (eval 1)) ; 1
(eval (quote (+ 1 2))) ; 3

(1) ; ERROR

; IO LISP

(display 1) ; 1
(display (+ 1 2)) ; 3
(begin (display 1) (display 2)) ; 12
(define three (begin (display 1) (display 2) 3)) ; 12 VOID
three ; 3

; STRING LISP

"string" ; "string"
(string? "string") ; #t

; LOADING LISP

(load "/home/rohan/sw/hsc3-forth/lisp/stdlib.lisp")

; SICP

(define square (lambda (n) (* n n))) ; VOID

(define f
  (lambda (x y)
    ((lambda (a b) (+ (+ (* x (square a)) (* y b)) (* a b)))
     (+ 1 (* x y))
     (- 1 y))))

(f 7 9) ; 28088
