; DEFINE

(set! define-rw (λ exp (cons 'set! (cdr exp))))
(set! define (macro define-rw))

; NIL

(define nil '())

; LAMBDA

(define lambda-rw*-code
  (quote
   (λ exp
     (let ((param (car exp))
           (code (car (cdr exp))))
       (cond ((null? param) (list 'λ '_ code))
             ((null? (cdr param)) (list 'λ (car param) code))
             (else (list 'λ (car param) (lambda-rw* (list (cdr param) code)))))))))

; (expand lambda-rw*-code)
(define lambda-rw* (λ exp ((λ param ((λ code (if (null? param) (cons (quote λ) (cons (quote _) (cons code nil))) (if (null? (cdr param)) (cons (quote λ) (cons (car param) (cons code nil))) (cons (quote λ) (cons (car param) (cons (lambda-rw* (cons (cdr param) (cons code nil))) nil)))))) (car (cdr exp)))) (car exp))))

(define lambda-rw (λ exp (lambda-rw* (cdr exp))))

(define lambda (macro lambda-rw))

; LET

(define let-rw*-code
  (quote
   (λ exp
     (if (null? (car exp))
         (cadr exp)
         (list (list 'λ (caaar exp) (let-rw* (list (cdar exp) (cadr exp))))
               (cadr (caar exp)))))))

; (expand let-rw*-code)
(define let-rw* (λ exp (if (null? (car exp)) (cadr exp) (cons (cons (quote λ) (cons (caaar exp) (cons (let-rw* (cons (cdar exp) (cons (cadr exp) nil))) nil))) (cons (cadr (caar exp)) nil)))))

(define let-rw (λ exp (let-rw* (cdr exp))))

(define let (macro let-rw))

; LIST

; list is a macro because HSC3-LISP lambda is (a -> b) and not VARARG
(define list-rw
  (λ exp
    (let ((f (lambda (e r) (append (cons 'cons (cons e nil)) (cons r '())))))
      (foldr f 'nil (cdr exp)))))

(define list (macro list-rw))

; LOGIC

(define not (λ p (if p #f #t)))

(define and-rw (λ exp (list 'if (cadr exp) (caddr exp) #f)))
(define and (macro and-rw))

(define or-rw (λ exp (list 'if (cadr exp) #t (caddr exp))))
(define or (macro or-rw))

(define cond-rw
  (λ exp
    (let ((c (cdr exp)))
      (if (null? c)
          nil
          (let ((c0 (car c)))
            (if (equal? (car c0) 'else)
                (cadr c0)
                (list 'if (car c0) (cadr c0) (cond-rw (cons 'cond (cdr c))))))))))

(define cond (macro cond-rw))

(define when-rw
  (λ exp
    (let ((test (cadr exp))
          (branch (caddr exp)))
      (list 'if test branch nil))))

(define when (macro when-rw))

; BEGIN

(define begin-rw*-code
  (quote
   (lambda (pre code)
     (if (null? code)
         pre
         (begin-rw* (list (list 'λ '_ (car code)) pre) (cdr code))))))

; (expand begin-rw*-code)
(define begin-rw* (λ pre (λ code (if (null? code) pre (begin-rw* (cons (cons (quote λ) (cons (quote _) (cons (car code) nil))) (cons pre nil)) (cdr code))))))

(define begin-rw (λ exp (begin-rw* nil (cdr exp))))

(define begin (macro begin-rw))

; EXPAND

(define expand
  (λ exp
    (if (list? exp)
        (let ((f (λ nm (equal? (car exp) nm))))
          (cond ((f 'list) (expand (list-rw (map expand exp))))
                ((f 'let) (expand (let-rw (map expand exp))))
                ((f 'cond) (expand (cond-rw (map expand exp))))
                ((f 'lambda) (expand (lambda-rw (map expand exp))))
                ((f 'begin) (expand (begin-rw (map expand exp))))
                (else (map expand exp))))
        exp)))

; UID

(define uid 0)

(define incr-uid (λ n (begin (set! uid (+ uid n)) uid)))

; COMPAT

(define let* let)
(define mce2 (lambda (p q) (mce (list p q))))

; MATH

(define pi 3.141592653589793)

(define = equal?)

; IO

(define newline (λ _ (display "\n")))

; C....R

(define caar (λ c (car (car c))))
(define cadr (λ c (car (cdr c))))
(define cdar (λ c (cdr (car c))))
(define cddr (λ c (cdr (cdr c))))
(define caaar (λ c (car (car (car c)))))
(define caadr (λ c (car (car (cdr c)))))
(define cadar (λ c (car (cdr (car c)))))
(define caddr (λ c (car (cdr (cdr c)))))
(define cdaar (λ c (cdr (car (car c)))))
(define cdadr (λ c (cdr (car (cdr c)))))
(define cddar (λ c (cdr (cdr (car c)))))
(define cdddr (λ c (cdr (cdr (cdr c)))))
(define caaaar (λ c (car (car (car (car c))))))
(define caaadr (λ c (car (car (car (cdr c))))))
(define caadar (λ c (car (car (cdr (car c))))))
(define caaddr (λ c (car (car (cdr (cdr c))))))
(define cadaar (λ c (car (cdr (car (car c))))))
(define cadadr (λ c (car (cdr (car (cdr c))))))
(define caddar (λ c (car (cdr (cdr (car c))))))
(define cadddr (λ c (car (cdr (cdr (cdr c))))))
(define cdaaar (λ c (cdr (car (car (car c))))))
(define cdaadr (λ c (cdr (car (car (cdr c))))))
(define cdadar (λ c (cdr (car (cdr (car c))))))
(define cdaddr (λ c (cdr (car (cdr (cdr c))))))
(define cddaar (λ c (cdr (cdr (car (car c))))))
(define cddadr (λ c (cdr (cdr (car (cdr c))))))
(define cdddar (λ c (cdr (cdr (cdr (car c))))))
(define cddddr (λ c (cdr (cdr (cdr (cdr c))))))
