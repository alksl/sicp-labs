;;;
; Uppgift 3: Funktionsdefinitioner och värden
;;;

(define (max2 a b)
  (if (> a b)
      a
      b))

(define (max3 a b c)
  (max2 a (max2 b c)))

(max2 1 2)
;2

(max2 2 1)
;2

(max3 1 2 3)
;3

(max3 3 1 2)
;3

(max3 2 3 1)
;3

;;;
; Uppgift 4: Värden och namnrymder
;;;

(define (foo) (* 1 2 3 4)) 
(define foo (* 1 2 3 4)) 
(define (foobar) (* 1 2 3 4)) 

; foo evaluerar till värdet 24
; Substitution
;foo
;24

; foobar evaluerar till proceduren foobar
; Substitution
;foobar
;#<procedure:foobar>

; Försöker köra foo men det går inte eftersom att foo är ett tal
; och inte en procedur.
; Substitution
;(foo)
;(24)
;Booom!

; Kör funktionen foobar och returnerar talet 24
; Substitution
;(foobar)
;(* 1 2 3 4)
;24

; Det finns bara en foo.

;;;
; Uppgift 5: Scope (inledande)
;;;

;(define (powers-of-two expt) 
; (expt 2 expt)) 

; Substitution
;(powers of-two 4)
;(4 2 4)
; Boom!!

; Detta fungerar (powers-of-two (lambda (a b) 'nop))

; Ovanstående ger fel eftersom att exemplet försöker
; köra heltalet 4 med argumenten 2 4 (4 är inte en procedur)

(define (powers-of-two expt)
  (if (= expt 0)
      1
      (* 2 (powers-of-two (- expt 1)))))

(powers-of-two 4)
; 16

;;;
; Uppgift 6: Länkade listor
;;;

(define (occurs? x list)
  (if (null? list)
      #f
      (or (and 
           (not (list? (first list))) 
           (= x (first list)))
          (occurs? x (rest list)))))

(occurs? 1 `(3 4 5)) 
;#f

(occurs? 1 `(3 (4 1) 5)) 
;#f

(occurs? 1 `(2 3 1 4)) 
;#t

;;;
; Uppgift 7: map över listor
;;;

(define (my-map fn list)
  (if (null? list)
      '()
      (cons (fn (first list))
            (my-map fn (rest list)))))

(my-map sqrt '(1 4 9))
;(1 2 3)

(my-map list '(a b c))
;((a) (b) (c))

;;;
; Uppgift 8: Dubbelrekursion
;;;

(define (occurs-somewhere? x list)
  (cond ((null? list) #f)
        ((list? (first list))
         (or (occurs-somewhere? x (first list))
             (occurs-somewhere? x (rest list))))
        (else (or (= x (first list))
                  (occurs-somewhere? x (rest list))))))
      
(occurs-somewhere? 1 `(3 4 5)) 
;#f

(occurs-somewhere? 1 `(3 (4 1) 5)) 
;#t

(occurs-somewhere? 1 `(2 3 1 4)) 
;#t

;;;
; Uppgift 9: Listuppbyggande
;;;

(define (first-n n list)
  (if (or (null? list) (= n 0))
      '()
      (cons (first list)
            (first-n (- n 1) (rest list)))))

(first-n 2 '(3 4 5))
;(3 4)

(first-n 5 '(3 4 5))
;(3 4 5)

(first-n 0 '(3 4 5))
;()
