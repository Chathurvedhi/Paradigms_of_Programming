
(define iteration-space
  (lambda (l)
    (let
        ((a (car l))
         (b (car (cdr l)))
         (c (car (cdr (cdr l)))))
      (if (> a b)
          '()
          (cons a (iteration-space (cons (+ a c) (cdr l))))))))

(define make-iterator
  (lambda (l)
    (box (iteration-space l))))

(define next1
  (lambda (b)
    (display (car (unbox b)))
    (set-box! b (cdr (unbox b)))
  )
)

(define next
  (lambda (b)
    (cond
      ((empty? (unbox b)) '())
      (else (next1 b))
    )
  )
)

(define hasNext
  (lambda (b)
    (not (empty? (unbox b)))))
