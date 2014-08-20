
;;; --------------------------------------------------------------------------
;;;  lazy.ss
;;;  Lazy evaluator for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).

;;; Modifierad för Scheme version 6 (mutable cons). Thunk-objekten.
;; / AH 2009-11-25
;;; --------------------------------------------------------------------------
;; (load "TDDA69/Lab/meta_eval.ss")
;; (load "/home/TDDA69/Lab/r6rs/meta_eval.ss")
(load "meta_eval.ss")

;;; New core of the evaluator
;;; -------------------------

(define (eval-%scheme exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
;       ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-%scheme (cond->if exp) env))
        ((application? exp)
         (apply-%scheme (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error 'eval-%scheme "Unknown expression type ~s" exp))))

(define (actual-value exp env)
  (force-it (eval-%scheme exp env)))

(define (apply-%scheme procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (map parameter-name (procedure-parameters procedure))
           (list-of-delayed-args (procedure-parameters procedure) arguments env)
           (procedure-environment procedure))))
        (else
         (error 'apply-%scheme "Unknown procedure type ~s" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args params exps env)
  (cond ((no-operands? exps)
         '())
        ((parameter-type? (first params) '%lazy)
         (cons (delay-it (first-operand exps) env)
               (list-of-delayed-args (rest params)(rest-operands exps) env)))
        ((parameter-type? (first params) '%lazy-memo)
         (cons (delay-it-memo (first-operand exps) env)
               (list-of-delayed-args (rest params) (rest-operands exps) env)))
        ((parameter-type? (first params) '%value)
         (cons (actual-value (first-operand exps) env)
               (list-of-delayed-args (rest params) (rest-operands exps) env)))
        (else
         (error 'list-of-delayed-args "Invalid parameter passing method."))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval-%scheme (if-consequent exp) env)
      (eval-%scheme (if-alternative exp) env)))

(define (make-procedure parameters body env)
  (list '$procedure
        (map make-tagged-parameter parameters)
        body
        env))

;;; Representing thunks
;;; -------------------

;; To facilitate transformation between call-by-name and call-by-need the
;; implementation knows about two types of thunks: normal thunks (for
;; call-by-name) and memo-thunks (for call-by-need). The latter ones are
;; transformed into evaluated-thunks when evaluated the first time.

(define thunk '$thunk)

(define memo-thunk '$memo-thunk)

(define evaluated-thunk '$evaluated-thunk)

(define (delay-it exp env)
  (mcons thunk (mcons exp (mcons env '()))))

(define (delay-it-memo exp env)
  (mcons memo-thunk (mcons exp (mcons env '()))))

(define (thunk? obj)
  (tagged-list? obj thunk))

(define (memo-thunk? obj)
  (tagged-list? obj memo-thunk))

(define (thunk-exp thunk) (mcar (mcdr thunk)))

(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

(define (evaluated-thunk? obj)
  (tagged-list? obj evaluated-thunk))

(define (thunk-value evaluated-thunk) (mcar (mcdr evaluated-thunk)))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-mcar! obj evaluated-thunk)
           (set-mcar! (mcdr obj) result)  ; replace exp with its value
           (set-mcdr! (mcdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;;; New user interface
;;; ------------------

(define (driver-loop)
  (newline)
  (display "%==> ")
  (do ((%exp (read)(read)))
      ((memq %exp '(exit stop logout hej-d�))
       (display "Have a nice day!") (newline))
      (user-print (actual-value %exp the-global-environment))
      (newline)
      (display "%==> ")))

(define (user-print object)
  (define (circular-print object)
    (cond ((or (not (list? object)) (eq? object '())) object)
	  ((compound-procedure? object)
	   (list '<$procedure
		 (procedure-parameters object)
		 (procedure-body object)
		 '<procedure-environment>>))
	  ((primitive-procedure? object)
	   (list '<$primitive>))
	  ((thunk? object)
	   (list '<$thunk (thunk-exp object) '<thunk-environment>>))
	  ((memo-thunk? object)
	   (list '<$memo-thunk (thunk-exp object) '<thunk-environment>>))
	  (else (cons (circular-print (car object))
		      (circular-print (cdr object))))))
  (display (circular-print object)))

(define (init-%scheme)
  (set! the-global-environment (setup-environment))
  (load-%scheme "lab5.%ss")
  (load-%scheme "streams.%ss")
  (driver-loop))

(define (load-%scheme-expressions in)
  (let ((exp (read in)))
    (if (not (eof-object? exp))
        (begin
          (eval-%scheme exp the-global-environment)
          (load-%scheme-expressions in)))))

(define (load-%scheme filename)
  (define in (open-input-file filename))
  (load-%scheme-expressions in)
  (close-input-port in))

;; Useful abbreviations

(define init init-%scheme)
(define go go-%scheme)

;;; --------------------------------------------------------------------------

(display "Loaded lazy.ss")
(newline)
