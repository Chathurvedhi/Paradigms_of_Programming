(define mix
  (lambda (l1 l2)
    (if (null? l1)
        '()
        (let ((l '())) (cons (cons (car l1) (cons (car l2) l)) (mix (cdr l1) (cdr l2)))
        )
    )
  )
)

(define first
  (lambda (ll)
    (cond
      ((empty? ll) '())
      (else (cons (caar ll) (first (cdr ll))))
    )
  )
)

(define second
  (lambda (ll)
    (if (null? ll)
        '()
        (cons (cadar ll) (second (cdr ll))))))

(define unmix
  (lambda (ll)
    (if (null? ll)
        (cons '() (cons '() '()))
        (cons (first ll) (cons (second ll) '()))
    )
  )
)