(define remeq
(lambda (f x ll outl)
  (cond
    ((null? ll) outl)
    ((f x (car ll)) (remeq f x (cdr ll) outl))
    (else (remeq f x (cdr ll) (cons (car ll) outl)))
  )
)
)

(define addeq
(lambda (f x ll outl)
  (cond
    ((null? ll) outl)
    ((f x (car ll)) (addeq f x (cdr ll) (cons (car ll) outl)))
    (else (addeq f x (cdr ll) outl))
  )
)
)

(define group
(lambda (f ll outl)
  (if (null? ll)
      outl
      (group f (remeq f (car ll) (cdr ll) '()) (cons (addeq f (car ll) (cdr ll) (cons (car ll) '())) outl)))))
      


(define eqGroupify
(lambda (f ll)
  (group f ll '())))