; 3.1, 3.2, 3.3, 3.7, 3.8
; http://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html

; 3.1
(define (make-accumulator x)
  (let ((sum x))
    (lambda (amount)
      (begin (set! sum (+ sum amount))
             sum))))

; 3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (input)
      (cond ((equal? input 'how-many-calls?) count)
            ((equal? input 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1)) (f input)))))))

; 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw msg)
    (cond ((not (eq? pw password)) (lambda (x) "Incorrect password"))
          ((eq? msg 'withdraw) withdraw)
          ((eq? msg 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       msg))))
  dispatch)

; 3.7
(define (make-joint account password new-password)
  (lambda (pw msg)
    (if (eq? pw new-password)
        (account password msg)
        (lambda (x) "Incorrect password"))))

; 3.8
(define f
  (let ((count 0))
    (lambda (x)
      (cond ((and (= count 0) (= x 1)) (set! count 1) 1)
            (else 0)))))



