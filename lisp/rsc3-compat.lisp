;; COMPAT

(define dinf 9e8)

(define make-mce mce)
(define make-mrg (lambda (p q) (mrg (list p q))))

(define u:log log)
(define u:cos cos)
(define u:sin sin)

(define show-graph draw)

(define cps-midi cpsmidi)
(define midi-cps midicps)
