
;;; --------------------------------------------------------------------------
;;;  account.ss
;;;  Template for the bank account assignment in the course "Data and
;;;  Program Structures" (TDDA69).
;;; --------------------------------------------------------------------------

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error 'dispatch "Unknown request: ~s" m))))
  dispatch)

