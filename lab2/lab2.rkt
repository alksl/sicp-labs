(load "streams.ss")

;;;
; Uppgift 1
;;;

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (devisible? x n)
  (equal? (modulo x n) 0))

(define integers-not-devisable-by-2-3-5
  (stream-filter
   (lambda (x)
     (not (or (devisible? x 2)
              (devisible? x 3)
              (devisible? x 5))))
   integers))

(print-stream integers-not-devisable-by-2-3-5 20)
;[ 1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59 61 67 71 73 ... ]


; För deluppgift b se pdf

;;;
; Uppgift 2
;;;

(define not-devisible-by-3
  (stream-filter (lambda (x) (devisible? x 3)) integers))

(define not-devisible-by-7
  (stream-filter (lambda (x) (devisible? x 7)) integers))

(print-stream not-devisible-by-3 10)
;[ 3 6 9 12 15 18 21 24 27 30 ... ]

(print-stream not-devisible-by-7 10)
;[ 7 14 21 28 35 42 49 56 63 70 ... ]

(print-stream (interleave not-devisible-by-3 not-devisible-by-7) 20)
;[ 3 7 6 14 9 21 12 28 15 35 18 42 21 49 24 56 27 63 30 70 ... ]


;;;
; Uppgift 3
;;;

(define S1
  (cons-stream 
   1
   (merge (scale-stream 2 S1)
         (merge (scale-stream 3 S1)
                (scale-stream 5 S1)))))

(print-stream S1 14)
;[ 1 2 3 4 5 6 8 9 10 12 15 16 18 20 ... ]

; De 6 tal efter 405
(print-stream 
 (stream-filter (lambda (x) (> x 405)) S1)
 6)
;[ 432 450 480 486 500 512 ... ]

; Med stora tal så behöver man gå långt i strömmen med tal
; som inte är delbara med 2, 3 eller 5 i kontrollen när 
; man filtrerar. Vilket blir väldigt ineffektivt speciellt
; när det ska göras för alla tal.


;;;
; Uppgift 4
;;;

(define (cons-mstream head tail)
  (mcons head (delay tail)))

(define (stream-mcar stream)
  (mcar stream))

(define (stream-mcdr! stream)
  (set-mcdr! stream (force (mcdr stream)))
  (mcdr stream))

(define a
  (cons-mstream 
   1
   (cons-mstream 
    2
    (cons-mstream 3 the-empty-stream))))

; Först är a en ström med resten av listan fördröjd
a
;{1 . #<promise:...tdda69/lab2/lab2.rkt:78:14>}

; Hämta slutet på strömen
(stream-mcdr! (stream-mcdr! (stream-mcdr! a)))
;()

; När hela listan har traverserats har den även sparats
; och a är en (muterbar) lista. set-mcdr! har alltså 
; räknat ut och sparat värdet av strömen
a
;{1 2 3}

; Hämta ett sparat värde
(stream-mcar (stream-mcdr! a))
;2
