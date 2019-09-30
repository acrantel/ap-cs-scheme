;2.1 ugly way
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (negative? n) (negative? d))
           (cons (/ (- n) g) (/ (- d) g)))
          ((and (negative? n) (positive? d))
           (cons (/ n g) (/ d g)))
          ((and (positive? n) (negative? d))
           (cons (/ (- n) g) (/ (- d) g)))
          (else (cons (/ n g) (/ d g))))))

;2.2
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;2.7
(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

