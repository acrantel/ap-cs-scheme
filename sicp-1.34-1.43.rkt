;1.37a) 
(define (cont-frac n d k)
  (define (iter val i)
    (if (>= i 1)
        (iter (/ (n i) (+ (d i) val)) (- i 1))
        val))
  (iter 0 k))
;1/(golden ratio) = 0.61803398875
;k = 20 => 0.6180339850173578
;k = 15 => 0.6180344478216819
;k = 12 => 0.6180257510729613
;k = 11 => 0.6180555555555556

; 1.38)
(define (approx-e n)
  (+ 2 (cont-frac (lambda (x) 1)
                  (lambda (x) (if (= 0 (modulo (+ x 1) 3)) (* 2 (/ (+ x 1) 3)) 1)) n)))

; 1.41)
(define (double p)
  (lambda (x) (p (p x))))
(((double (double double)) inc) 5) => 21

; 1.42)
(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43)
(define (repeated f n)
  (define (iter a b i)
    (cond ((= i 0) a)
          ((even? i) (iter a (compose b b) (/ i 2)))
          (else (iter (compose a b) b (- i 1)))))
  (iter (lambda (x) x) f n))

