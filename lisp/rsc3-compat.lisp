;; COMPAT

(define dinf 9e8)

(define make-mce mce)
(define mce2 (lambda (p q) (mce (list p q))))

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

(define eq ==)
(define lt <)
(define gt >)

(define fdiv f-div)

(define show-graph draw)

(define cps-midi cpsmidi)
(define midi-cps midicps)
(define tw-index t-windex)

(define rand* (lambda (n) (rand 0 n)))

(define play-at
  (lambda (fd u nid act grp)
    (play-at* (list fd u nid act grp))))

(define reset (lambda (_) (stop)))

(define audition (lambda (u) (play-at nil u -1 add-to-head 1)))

(define with-sc3 (lambda (f) (f nil)))

(define +inf.0 9e8)

; actually rsc3 doesn't have these...

; the cardinality input is derived from the values input...
(define set-buf
  (lambda (buf offset values)
    (mk-ugen (list "SetBuf" ir (list buf (length values) offset) (make-mce values) 1 nil nil))))

(define as-local-buf
  (lambda (l)
    (let ((b (local-buf (length l) 1))
          (s (set-buf b 0 l)))
      (mrg2 b s))))
