(define create-symbol-table
  (lambda (ll) ll))

(define lookup
  (lambda (ll s)
    (cond
      ((null? ll) '())
      ((eq? (caar ll) s) (cadar ll))
      (else (lookup (cdr ll) s))
    )
  )
)

(define extend-symbol-table
  (lambda (l1 l2)
    (append l2 l1)))