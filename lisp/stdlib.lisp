; LIST

; list is a macro because HSC3-LISP is not VARARG
(define list-rw
  (lambda (l)
    (let ((f (lambda (e r)
               (append (cons 'cons (cons e nil)) (cons r '())))))
      (foldr f 'nil l))))

(define list (macro list-rw))

; LOGIC

(define not (lambda (p) (if p #f #t)))

(define and-rw (lambda (sexp) (list 'if (car sexp) (cadr sexp) #f)))
(define and (macro and-rw))

(define or-rw (lambda (sexp) (list 'if (car sexp) #t (cadr sexp))))
(define or (macro or-rw))

(define cond-rw
  (lambda (sexp)
    (if (null? sexp)
        void
        (let ((c0 (car sexp)))
          (if (equal? (car c0) 'else)
              (cadr c0)
              (list 'if (car c0) (cadr c0) (cond-rw (cdr sexp))))))))

(define cond (macro cond-rw))

(define when-rw
  (lambda (sexp)
    (let ((test (car sexp))
          (branch (cadr sexp)))
      (cons 'if (cons test (cons branch (cons void '())))))))

(define when (macro when-rw))

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
