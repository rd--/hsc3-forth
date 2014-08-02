(define pi 3.141592653589793)
(define newline (lambda (_) (display "\n")))

(define id (lambda (x) x))

; (++) :: [a] -> [a] -> [a]
(define append (lambda (a b) (if (null? a) b (cons (car a) (append (cdr a) b)))))

; (map (+ 1) (cons 1 (cons 2 '())))
(define map
  (lambda (f l)
    (if (null? l) '() (cons (f (car l)) (map f (cdr l))))))

; (foldl + 0 (cons 1 (cons 2 '())))
(define foldl
  (lambda (f z l)
    (if (null? l)
        z
        (foldl f (f z (car l)) (cdr l)))))

; (foldr cons '() (cons 1 (cons 2 '())))
(define foldr
  (lambda (f z l)
    (if (null? l)
        z
        (f (car l) (foldr f z (cdr l))))))

(define nil '())

;; (list 1 2 3) => (cons 1 (cons 2 (cons 3 '())))
; (list-rewriter '(1 2 3))
(define list-rewriter
  (lambda (l)
    (let ((f (lambda (e r) (append (cons 'cons (cons e nil)) (cons r '())))))
      (foldr f 'nil l))))

;;      (cons 'quote (cons (foldr f '() l) '())))))

(define list (macro list-rewriter))

; (when-rewriter '(#t (display "TRUE")))
; (when-rewriter '(#f (display "FALSE")))
(define when-rewriter
  (lambda (sexp)
    (let ((test (car sexp))
          (branch (cadr sexp)))
      (cons 'if (cons test (cons branch (cons void '())))))))

;; (when #t (display "TRUE\n"))
(define when (macro when-rewriter))

; (and p q) => (if p q #f)
; (define and (macro (lambda (sexp) (cons 'if (cons (car l) (
; (or p q) => (if p #t q)

(define length (lambda (l) (if (null? l) 0 (+ 1 (length (cdr l))))))

;; cxr
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
