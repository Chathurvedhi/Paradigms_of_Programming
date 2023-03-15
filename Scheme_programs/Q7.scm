(define fib
  (lambda (x prev curr)
    (if (zero? x)
        prev
        (fib (- x 1) curr (+ curr prev))
    )
  )
)

(define iter-fib
  (lambda (x) (fib x 0 1)))
