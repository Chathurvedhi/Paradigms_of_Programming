(define iteration-space
  (lambda (l)
    (let
        ((a (car l))
        (b (car (cdr l)))
        (c (car (cdr (cdr l)))))
      (if (> a b)
          '()
          (cons a (iteration-space (cons (+ a c) (cdr l))))))))