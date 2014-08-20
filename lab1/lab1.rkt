(load "higher-order.ss")
(load "constraints.ss")

;;;
; Uppgift 1: Applicative order vs normal order
;;;

;(define (p) (p))
;(define (test x y)
;  (if (= x 0) 0 y))

;(define (p) (p))

;(define (test x y)
;  (if (= x 0) 
;    0 
;    y))

;(test 0 (p))

; Vid normal order så kommer uttrycken bytas ut
; och sedan kommer proceduren p inte evalueras 
; eftersom att (= 0 0) är #'t
; 
; (test 0 (p)) 
; (if (= 0 0) 0 (p))
; 0


; Vid Applicative order så kommer argumenten 
; först evalueras och sedan appliceras, dvs.
; det andra argumentet kommer att orsaka
; oändling rekusion eftersom att den kallar
; sig själv utan slutvilkor
;
; (test 0 (p))
; .
; .
; .

;;;
; Uppgift 2: Special forms
;;;

;(define (new-if predicate then-clause else-clause)
; (cond (predicate then-clause)
; (else else-clause)))
;
;(define (sqrt-iter guess x) 
;  (new-if (good-enough? guess x) 
;         guess 
;         (sqrt-iter (improve guess x) x)))

; I sqrt-iter kommer alla argument evalueras först inklusive
; det rekursiva anropet (sqrt-iter ...). Vilket orsakar oändlig
; rekursion. If måste därför vara en specialform som endast 
; evaluerar sina argument efter behov.


;;;
; Uppgift 3: Rekursiva och iterativa processer
;;;

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))

(sum-iter (lambda (x) x) 
          1
          (lambda (x) (+ x 1)) 
          5)
;15

;;;
; Uppgift 4: Högre ordningens funktioner
;;;

(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a)
         (product factor (next a) next b))))

(define (product-iter factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (factor a) result))))
  (iter a 1))

(product
 (lambda (x) x)
 1
 (lambda (x) (+ x 1))
 5)
;120

(product-iter 
 (lambda (x) x)
 1
 (lambda (x) (+ x 1))
 5)
;120

(define (factorial n)
  (product-iter
   (lambda (x) x)
   1
   (lambda (x) (+ 1 x))
   n))

(factorial 6)
;720

(define (my-pi n)
  (exact->inexact
   (* 4
      (product-iter
       (lambda (x)
         (if (even? x)
             (/ x (+ x 1))
             (/ (+ x 1) x)))
       2
       (lambda (x) (+ x 1))
       n))))

(my-pi 20)
;3.067703806643499

(my-pi 1000)
;3.1400238186005973

(my-pi 10000)
;3.1414355935899083

;;;
; Uppgift 5: Generalisering
;;;

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate 
                          combiner
                          null-value
                          term
                          (next a)
                          next
                          b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
   (accumulate + 0 term a next b)) 

(define (product term a next b)
  (accumulate * 1 term a next b))

(sum (lambda (x) x) 
     1
     (lambda (x) (+ x 1)) 
     5)
;15

(product (lambda (x) x) 
         1
         (lambda (x) (+ x 1)) 
         5)
;120

; Eftersom att argumenten till combiner appliceras i omvänd
; ordning (jämförelsevis med den rekursiva) med den▫
; iterativa processlösningen, dvs. funktioner som beror
; på appliceringsordning ge olika resultat.
;
; Detta kan illustreras med tex. cons som ger en "omvänd"
; lista med den iterativa processlösningen.
;

(define (add-one x) (+ 1 x))

(accumulate cons null identity 1 add-one 5)
;'(1 2 3 4 5)

(accumulate-iter cons null identity 1 add-one 5)
;'(5 4 3 2 1)

;;;
; Uppgift 6: Yttligare generalisering
;;;

(define (filter-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filter-accumulate filter
                                                combiner
                                                null-value
                                                term
                                                (next a)
                                                next
                                                b))
          (filter-accumulate filter
                             combiner
                             null-value
                             term
                             (next a)
                             next
                             b))))

(define (sum-prime-squares a b)
  (filter-accumulate prime? + 0 square a add-one b))

(sum-prime-squares 1 10)
;87

(define (relative-prime? x y)
  (equal? 1 (gcd x y)))

(define (mult-relative-primes n)
  (filter-accumulate (lambda (x) (relative-prime? x n))
                       *
                       1
                       identity
                       1
                       add-one
                       n))

(mult-relative-primes 10)
;189

;;;
; Uppgift 7: fold-mönstret
;;;

(define (my-foldl fn initval list)
  (define (iter list result)
    (if (null? list)
        result
        (iter (rest list) (fn (first list) result))))
  (iter list initval))

(my-foldl cons '() '(1 2 3 4))
;(4 3 2 1)

(define (my-foldr fn initval list)
  (if (null? list)
      initval
      (fn (first list)
          (my-foldr fn initval (rest list)))))

(my-foldr cons '() '(1 2 3 4))
;(1 2 3 4)

(define (my-map-fold f seq)
  (my-foldr (lambda (x y) (cons (f x) y)) '() seq))

(my-map-fold square '(1 2 3 4))
;(1 4 9 16)

(define (reverse-r sequence)
  (my-foldr (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-l sequence)
  (my-foldl cons '() sequence))


(reverse-r '(1 2 3 4))
;(4 3 2 1)

(reverse-l '(1 2 3 4))
;(4 3 2 1)

;;;
; Uppgift 8: Högre ordningens funktioner igen
;;;

(define (repeated fn n)
  (lambda (x)
    (define (repeat a)
      (fn
       (if (= a 1)
           x
           (repeat (- a 1)))))
    (repeat n)))

((repeated square 2) 5)
;625

((repeated add-one 7) 5)
;12

((repeated add-one 7) 0)
;7

(define (my-compose f g)
  (lambda args (f (apply g args))))
  
((my-compose identity +) 1 2 3)
;6

;;;
; Uppgift 9: Högre ordningens funktioner för sista gången
;;;

(define (smooth fn)
  (lambda (x)
    (/ (+ (fn x) (fn (+ x 0.001)) (fn (- x 0.001))) 3)))

((smooth sin) 0.456)
;0.44036020816641036

(define (n-fold-smooth fn n)
  ((repeated smooth n) fn))

((n-fold-smooth sin 5) 0.456)
;0.44035962101980863

((n-fold-smooth square 5) 4)
;16.000003333333336

;;;
; Uppgift 10: Bonkkontoexemplet
;;;

(define (make-account balance time interestrate)
  (define (calculate-interest now)
    (* balance (- now time) interestrate))

  (define (update-balance-with-interest now)
    (set! balance (+ balance (calculate-interest now)))
    (set! time now))

  (define (check-time now action)
    (if (>= now time)
      (action)
      "Too late"))

  (define (withdraw amount now)
    (check-time
      now
      (lambda ()
        (begin
          (update-balance-with-interest now)
          (if (>= balance amount)
              (begin 
                (set! balance (- balance amount))
                balance)
              "Insufficient funds")))))

  (define (deposit amount now)
    (check-time
      now
      (lambda ()
        (begin
          (update-balance-with-interest now)
          (set! balance (+ balance amount))
          balance))))

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error 'dispatch "Unknown request: ~s" m))))

  dispatch)

(define my-account (make-account 1000 100 0.10))

((my-account 'withdraw) 100 110)
;1900.0

((my-account 'deposit)   100 120)
;3900.0

((my-account 'deposit)   100 110)
;"Too late"

;;;
; Uppgift 11: Omgivningsdiagram
;;;

; Se pdf


;;;
; Uppgift 12: Temperaturkonvertering
;;;

(define (average a b c)
  (let ((sum (make-connector))
        (half (make-connector)))
    (adder a b sum)
    (constant (/ 1 2) half)
    (multiplier sum half c))
  'ok)

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(average a b c)
;ok

(probe "Average" c)
;#<procedure:me>

(set-value! a 2 'user)
;done

(set-value! b 2 'user)
;Probe: Average = 2
;done

(forget-value! b 'user)
;Probe: Average = ?
;done

(set-value! b 10 'user)
;Probe: Average = 6
;done
                             
;;;
; Uppgift 13: Ny primitiv
;;;

(define (squarer a square)
  (define (process-new-value)
   (cond  ((has-value? a)
            (set-value!  
              square 
              (* (get-value a) (get-value a)) me))
          ((has-value? square)
            (if (> (get-value square) 0)
              (set-value! a (sqrt (get-value square)) me)
              (error "square less than 0 -- SQUARER")))
          (else
            (error "Cant process-new-value -- SQUARER"))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! square me))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
            (process-new-value))
          ((eq? request 'I-lost-my-value)
            (process-forget-value))
          (else
            (error 'squarer "Unknown request: ~s" request))))

  (connect a me)
  (connect square me)
  me)

(define (squarer2 a square)
  (multiplier a a square))

(define a (make-connector))
(define square (make-connector))

(squarer a square)
;(squarer2 a square)

(probe "A" a)
(probe "SQUARE" square)

; Det problem som finns för att implementera
; squarer är att om man bara tar hänsyn till 
; reella tal så är opreationen "square" en
; envägs funktion dvs. det finns inget tal som
; ger kvadraten -25.


(set-value! a 5 'user)
;Probe: A = 5
;Probe: SQUARE = 25
;done

(forget-value! a 'user)
;Probe: A = ?
;Probe: SQUARE = ?
;done

(set-value! square 25 'user)
;Probe: SQUARE = 25
;Probe: A = 5
;done

(forget-value! square 'user)
;Probe: SQUARE = ?
;Probe: A = ?
;'done

(set-value! square -25 'user)
;Probe: SQUARE = -25
;square less than 0 -- SQUARER
