(define dr 'dr)
(define ir 'ir)
(define kr 'kr)
(define ar 'ar)

(define clone (lambda (n u) (clone* (list (incr-uid 1) n u))))

; UID

(define uid 0)

(define incr-uid (λ n (begin (set! uid (+ uid n)) uid)))

