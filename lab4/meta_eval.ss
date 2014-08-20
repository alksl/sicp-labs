
;;; --------------------------------------------------------------------------
;;;  meta_eval.ss
;;;  Evaluator for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------

(load "abssyntax.ss")
(load "environment.ss")

;;; Core of the evaluator
;;; ---------------------

(define (eval-%scheme exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((defmacro? exp) (eval-defmacro exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-%scheme (cond->if exp) env))
        ((application? exp)
         (expand-and-apply exp env))
        (else
         (error 'eval-%scheme "Unknown expression type: ~s" exp))))

(define (expand-and-apply exp env)
  (let ((op (eval-%scheme (operator exp) env)))
    (if (macro? op)
        (apply-macro op (operands exp) env)
        (apply-%scheme (eval-%scheme (operator exp) env)
                       (list-of-values (operands exp) env)))))

(define (apply-%scheme procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error 'apply-%scheme "Unknown procedure type: ~s" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval-%scheme (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval-%scheme (if-predicate exp) env))
      (eval-%scheme (if-consequent exp) env)
      (eval-%scheme (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval-%scheme (first-exp exps) env))
        (else (eval-%scheme (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval-%scheme (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval-%scheme (definition-value exp) env)
                    env)
  'ok)

(define (eval-defmacro exp env)
  (define-variable!
    (defmacro-variable exp)
    (make-macro (defmacro-expander exp) env)
    env)
  'ok)

;;; Representing procedure objects
;;; ------------------------------

(define (make-procedure parameters body env)
  (list '$procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p '$procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc '$primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;;; Representing macro objects
;;; --------------------------

(define (make-macro expander env)
  (list '$macro
        (make-procedure
         (lambda-parameters expander)
         (lambda-body expander)
         env)))

(define (macro? m)
  (tagged-list? m '$macro))

(define (macro-procedure macro)
  (cadr macro))

(define (expand-macro macro arguments)
  (apply-%scheme (macro-procedure macro)
                 arguments))

(define (apply-macro macro arguments env)
  (eval-%scheme
   (expand-macro macro arguments)
   env))

(define (expand-%let %exp env)
  (let ((%let (lookup-variable-value '%let env)))
    (cond ((null? %exp) '())
          ((atom? %exp) %exp)
          ((tagged-list? %exp '%let) (expand-%let (expand-macro %let (cdr %exp)) env))
          (else (cons (expand-%let (car %exp) env) (expand-%let (cdr %exp) env))))))

;;; User interface
;;; --------------

(define (driver-loop)
  (newline)
  (display "%==> ")
  (do ((%exp (read)(read)))
      ((memq %exp '(exit stop logout hej-då))
       (display "Have a nice day!") (newline))
      (user-print (eval-%scheme (expand-%let %exp the-global-environment) the-global-environment))
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
	  (else (cons (circular-print (car object))
		      (circular-print (cdr object))))))
  (display (circular-print object)))

;; (init-%scheme) initializes the global environment, and calls driver-loop.
;; To start %Scheme without initializing, call (go-%scheme).

(define (init-%scheme)
  (set! the-global-environment (setup-environment))
  (load-%scheme "lab4.%ss")
  (load-%scheme "streams.%ss")
  (driver-loop))

(define (go-%scheme)
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

;; (remote-eval exp) makes it possible to evaluate %scheme-expressions from
;; Chez Scheme. The main use of this is to make definitions in the global
;; environment, without entering the interactive driver-loop.

(define (remote-eval %-exp)
  (user-print (eval-%scheme %-exp the-global-environment))
  (newline))

;;; Examples
;;; --------

;; Here are some examples that you may want to try out:

;; (remote-eval '(%define (fact n) (%if (%= 1 n) 1 (%* n (fact (%- n 1))))))
;; (remote-eval '(fact 10))


;;; --------------------------------------------------------------------------

(display "Loaded meta_eval.ss")
(newline)

(set! the-global-environment (setup-environment))

;(remote-eval
; '(%define a (%quote some-value)))

;(remote-eval
; '(%setq! a (%+ 2 3)))

