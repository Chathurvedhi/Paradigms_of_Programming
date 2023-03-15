(define bracket-no
  (lambda (l)
    (cond
      ((not (list? l)) 0)
      ((empty? l) 1)
      (else (+ (apply max (map bracket-no l)) 1))
    )
  )
)

(define bracketify
  (lambda (l x)
    (if (zero? x)
        l
        (cons (bracketify l (- x 1)) '())
    )
  )
)

(define list-int
  (lambda (l)
    (cond
      ((not (list? l)) (cons l '()))
      ((empty? l) '())
      (else (append (list-int (car l)) (list-int (cdr l))))
    )
  )
)

(define push-element
  (lambda (l)
    (bracketify (list-int l) (- (bracket-no l) 1))))