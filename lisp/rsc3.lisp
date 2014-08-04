;; COMPAT

(define dinf 9e8)

(define make-mce mce)

(define mce3 (lambda (a b c) (make-mce (list a b c))))
(define mce4 (lambda (a b c d) (make-mce (list a b c d))))

(define u:log log)
(define u:cos cos)
(define u:sin sin)

(define add3 sum3)
(define add4 sum4)

(define mul3 (lambda (p q r) (mul p (mul q r))))

(define show-graph draw)

(define cps-midi cpsmidi)
(define midi-cps midicps)
