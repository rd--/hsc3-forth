; DEFINE

(set! define-rw (lambda (exp) (cons 'set! (cdr exp))))
(set! define (macro define-rw))

; LET

; using list, which isn't available when let-rw first runs...
(define let-rw*
  (lambda (exp)
    (if (null? (cadr exp))
        (caddr exp)
        (list (list 'lambda
                    (list (caaadr exp))
                    (let-rw (list 'let (cdadr exp) (caddr exp))))
              (cadr (caadr exp))))))

; expanded
(define let-rw
  (lambda (exp)
    (if (null? (cadr exp))
        (caddr exp)
        (cons (cons 'lambda
                    (cons (cons (caaadr exp) nil)
                          (cons (let-rw (cons 'let (cons (cdadr exp) (cons (caddr exp) nil)))) nil)))
              (cons (cadr (caadr exp)) nil)))))

(define let (macro let-rw))

; LIST

; list is a macro because HSC3-LISP is not VARARG
(define list-rw
  (lambda (exp)
    (let ((f (lambda (e r)
               (append (cons 'cons (cons e nil)) (cons r '())))))
      (foldr f 'nil (cdr exp)))))

(define list (macro list-rw))

; LOGIC

(define not (lambda (p) (if p #f #t)))

(define and-rw (lambda (exp) (list 'if (cadr exp) (caddr exp) #f)))
(define and (macro and-rw))

(define or-rw (lambda (exp) (list 'if (cadr exp) #t (caddr exp))))
(define or (macro or-rw))

(define cond-rw
  (lambda (exp)
    (let ((c (cdr exp)))
      (if (null? c)
          nil
          (let ((c0 (car c)))
            (if (equal? (car c0) 'else)
                (cadr c0)
                (list 'if (car c0) (cadr c0) (cond-rw (cons 'cond (cdr c))))))))))

(define cond (macro cond-rw))

(define when-rw
  (lambda (exp)
    (let ((test (cadr exp))
          (branch (caddr exp)))
      (list 'if test branch nil))))

(define when (macro when-rw))

; UID

(define uid 0)

(define incr-uid (lambda (n) (begin (set! uid (+ uid n)) uid)))

; COMPAT

(define let* let)
(define mce2 (lambda (p q) (mce (list p q))))

; MATH

(define pi 3.141592653589793)

(define = equal?)

; IO

(define newline (lambda (_) (display "\n")))

; C....R

(define caar (lambda (c) (car (car c))))
(define cadr (lambda (c) (car (cdr c))))
(define cdar (lambda (c) (cdr (car c))))
(define cddr (lambda (c) (cdr (cdr c))))
(define caaar (lambda (c) (car (car (car c)))))
(define caadr (lambda (c) (car (car (cdr c)))))
(define cadar (lambda (c) (car (cdr (car c)))))
(define caddr (lambda (c) (car (cdr (cdr c)))))
(define cdaar (lambda (c) (cdr (car (car c)))))
(define cdadr (lambda (c) (cdr (car (cdr c)))))
(define cddar (lambda (c) (cdr (cdr (car c)))))
(define cdddr (lambda (c) (cdr (cdr (cdr c)))))
(define caaaar (lambda (c) (car (car (car (car c))))))
(define caaadr (lambda (c) (car (car (car (cdr c))))))
(define caadar (lambda (c) (car (car (cdr (car c))))))
(define caaddr (lambda (c) (car (car (cdr (cdr c))))))
(define cadaar (lambda (c) (car (cdr (car (car c))))))
(define cadadr (lambda (c) (car (cdr (car (cdr c))))))
(define caddar (lambda (c) (car (cdr (cdr (car c))))))
(define cadddr (lambda (c) (car (cdr (cdr (cdr c))))))
(define cdaaar (lambda (c) (cdr (car (car (car c))))))
(define cdaadr (lambda (c) (cdr (car (car (cdr c))))))
(define cdadar (lambda (c) (cdr (car (cdr (car c))))))
(define cdaddr (lambda (c) (cdr (car (cdr (cdr c))))))
(define cddaar (lambda (c) (cdr (cdr (car (car c))))))
(define cddadr (lambda (c) (cdr (cdr (car (cdr c))))))
(define cdddar (lambda (c) (cdr (cdr (cdr (car c))))))
(define cddddr (lambda (c) (cdr (cdr (cdr (cdr c))))))
