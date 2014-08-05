(define dr 'dr)
(define ir 'ir)
(define kr 'kr)
(define ar 'ar)

(define clone (lambda (n u) (clone* (list (incr-uid 1) n u))))
