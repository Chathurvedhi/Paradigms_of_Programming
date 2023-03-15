(define make-setty-cond
(lambda (x ll)
  (cond
    ((null? ll) '())
    ((= x (car ll)) (make-setty-cond x (cdr ll)))
    (else (cons (car ll) (make-setty-cond (car ll) (cdr ll))))
  )
)
)

(define setty
(lambda (x ll)
  (cons x (make-setty-cond x ll))))

(define make-setty-list
(lambda (ll)
  (if (null? ll)
      '()
      (setty (car ll) (cdr ll))
  )
)
)

(define setty-list-union
(lambda (l1 l2)
  (make-setty-list (append l1 l2))))

(define remove-ele
(lambda (x ll)
  (cond
    ((null? ll) '())
    ((= x (car ll)) (cdr ll))
    (else (cons (car ll) (remove-ele x (cdr ll)))))))

(define setty-list-minus
(lambda (l1 l2)
  (if (null? l2)
      l1
      (setty-list-minus (remove-ele (car l2) l1) (cdr l2)))))
