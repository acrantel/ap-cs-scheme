;3.17
(define (cp pair)
  (let ((visited '()))
    (define (visited? pair vl)
      (cond ((null? vl) #f)
            ((eq? pair (car vl)) #t)
            (else (visited? pair (cdr vl)))))
      ; return #t if the pair is in vl, #f otherwise
      ; the member function is NOT useful for this because it does
      ;   not check to see if the pair being searched is the exact
      ;   same memory location as a pair in the list
      ; note that we need the second input to receive visited
      ;   so we can cdr through its contents
      ; #t is used as a placeholder for real code
    (define (cp2 pair)
      (cond ((not (pair? pair)) 0)
            ((visited? pair visited) 0)
            (else (set! visited (cons pair visited))
                  (+ 1 (cp2 (car pair)) (cp2 (cdr pair))))))
      ; if not a pair, 0
      ; if already visited, 0
      ; else:
          ; updated visited (use set!) to include pair
          ; set! is not a placeholder--it is real code!
          ;(set! visited (cons pair visited))
          ; recursive calls on car and cdr
      ; 0 is used as a placeholder for real code
    (cp2 pair)))


;3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    ; empty-queue? is written to align with the way front-ptr
    ; and rear-ptr were given, above
    (define (empty-queue?)
      (null? front-ptr))

    ; peek returns the datum at the front of the queue
    ; peek returns #f if the queue is empty
    (define (peek)
      (cond ((empty-queue?) (error "Empty queue.  :-("))
            (else (car front-ptr))))

    ; insert-queue! plays out differently depending on whether the queue
    ; is currently empty or not
    (define (insert-queue! datum)
      (let ((new-node (cons datum '())))
        (cond ((empty-queue?) (set! front-ptr new-node)
                              (set! rear-ptr new-node))
              (else (set-cdr! rear-ptr new-node)
                    (set! rear-ptr new-node)))))

    ; delete-queue! has three possibilties:
    ; * empty queue
    ; * one element in queue
    ; * more than one element in queue
    (define (delete-queue!)
      (cond ((empty-queue?) (error "Empty queue.  :-("))
            (else 
                  ; store the datum at the head of the queue
                  (let ((return-value (peek)))
                    ; update the front pointer
                    (set! front-ptr (cdr front-ptr))
                    ; If there was only one thing in the queue, then the
                    ; rear-ptr will need to be set to nil
                    (if (null? front-ptr) (set! rear-ptr '()))
                    ; Now return the element of the queue (or #f)
                    return-value))))

    (define (dispatch message)
      (cond ((eq? message 'insert-queue!) insert-queue!)
            ((eq? message 'delete-queue!) delete-queue!)
            ((eq? message 'peek) peek)
            ((eq? message 'empty?) empty-queue?)))
    dispatch))

(define q (make-queue))
(define peek (q 'peek))
(define insert (q 'insert-queue!))
(define del (q 'delete-queue!))
(define empty? (q 'empty?))

;delete! problem for tables
(define (make-table)
  (cons '* '()))
(define (empty-table? t) (null? (cdr t)))

(define (insert! key value table)
  (let ((record (associated key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

        
(define (lookup k t)
  (let ((record (associated k (cdr t))))
    (cond (record (cdr record))
          (else #f))))
(define (associated key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (associated key (cdr records)))))

(define (rlookup k t)
  (let ((record (rassociated k (cdr t))))
    (cond (record (car record))
          (else #f))))
(define (rassociated value records)
  (cond ((null? records) #f)
        ((equal? value (cdar records)) (car records))
        (else (rassociated value (cdr records)))))

(define (delete! k t)
  (define (delete-iter k records)
    (cond ((null? (cdr records)) #f)
          ((equal? k (caadr records)) (let ((removed (cadr records)))
                                        (set-cdr! records (cddr records))
                                        removed))
          (else (delete-iter k (cdr records)))))
  (delete-iter k t))

;3.25
(define (make-table)
  (cons '* '()))
(define (empty-table? t) (null? cdr t))

(define (insert! keys value table)
  (if (null? keys)
      #f
      (begin (set-cdr! table (cons (cons keys value) (cdr table)))
             'ok)))

(define (lookup k t)
  (let ((record (associated k (cdr t))))
    (cond (record (cdr record))
          (else #f))))
(define (associated key records)
  (cond ((null? records) #f)
        ((member key (caar records)) (car records))
        (else (associated key (cdr records)))))
