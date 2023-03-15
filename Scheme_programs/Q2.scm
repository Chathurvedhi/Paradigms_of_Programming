(define iteration-space
  (lambda (l)
    (let
        ((a (car l))
        (b (car (cdr l)))
        (c (car (cdr (cdr l)))))
      (if (> a b)
          '()
          (cons a (iteration-space (cons (+ a c) (cdr l))))))))

(define for-loop (lambda (f l) (map f (iteration-space l))))