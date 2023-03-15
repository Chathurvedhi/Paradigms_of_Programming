
(define val
  (lambda (ll x)
    (cond
      ((zero? x) (car ll))
      (else (val (cdr ll) (- x 1))))))

(define find
  (lambda (ll x pos)
    (cond
      ((= (car ll) x) pos)
      (else (find (cdr ll) x (+ pos 1))))))

(define max-pos
  (lambda (f ll)
    (let(
         (pos 1)
         (l (map f ll))
         (x (apply max (map f ll)))
        )
      (find l x pos)
    )
  )
)
        

(define maximum-argument
  (lambda (f ll) (val ll (- (max-pos f ll) 1))))
    
