;; COMPAT

(define dinf 9e8)

(define make-mce mce)
(define make-mrg (lambda (p q) (mrg (list p q))))

(define u:abs abs)
(define u:cos cos)
(define u:floor floor)
(define u:log log)
(define u:max max)
(define u:min min)
(define u:mod mod)
(define u:round round)
(define u:sin sin)

(define string=? equal?)

(define lt <)
(define gt >)

(define fdiv f-div)

(define show-graph draw)

(define cps-midi cpsmidi)
(define midi-cps midicps)
(define tw-index t-windex)

(define rand* (lambda (n) (rand 0 n)))

(define with-sc3 (macro (lambda (_) '(stop))))
