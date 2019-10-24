(define (three-factor-triangular-nums)
  ; x is the first number in the consecutive multiplication
  (define (iter x result)
    ; n is the last number in the triangular sum
    (let ((n (/ (+ -1 (sqrt (+ 1 (* 8 x (+ x 1) (+ x 2))))) 2)))
      (cond ((> x 636) result)
            ((integer? n) (iter (+ x 1) (append result (list (list n x (* x (+ x 1) (+ x 2)))))))
            (else (iter (+ x 1) result)))))
  (iter 1 '()))
