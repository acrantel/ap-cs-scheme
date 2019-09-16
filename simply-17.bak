;17.8) member
(define (member m lst)
  (cond ((not (list? lst)) #f)
        ((equal? m (car lst)) lst)
        (else (member (cdr lst)))))

;17.9) listref
(define (list-ref lst i)
  (if (= i 0)
      (car lst)
      (list-ref (cdr lst) (- i 1))))

;17.10) length
(define (length lst)
  (define (length-iter ct current)
    (if (null? current)
        ct
        (length-iter (+ 1 ct) (cdr current))))
  (length-iter 0 lst))

;17.11) before-in-list?
(define (before-in-list? lst e1 e2)
  (if (> (length (member e1 lst)) (length (member e2 lst)))
      #t
      #f))

;17.12) flatten
(define (flatten lst)
  (cond ((null? lst) lst)
        ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
        (else (cons (car lst) (flatten (cdr lst))))))

;17.14) branch
(define (branch nums lst)
  (if (null? nums)
      lst
      (branch (cdr nums) (list-ref lst (- (car nums) 1)))))
