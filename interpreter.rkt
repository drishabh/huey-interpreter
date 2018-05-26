;;  ------------------------------------------------------------------------
;; |   FILE           :  interpreter.rkt                                    |
;; |   AUTHOR         :  Rishabh Dalal                                      |
;; |   CREATION DATE  :  2018/04/11                                         |
;; |   DESCRIPTION    :  A collection of the functions we have written to   |
;; |                     process programs written in new little language.   |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2018/04/19  Rishabh Dalal                          |
;; |   DESCRIPTION    :  Added 'varref and 'color-in in the 'preprocess.    |
;; |                     Added 'color-in? and 'is-keyword? to eval-exp.     |
;; |                     Wrote 'run-huey to take input and evaluate it.     |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2018/04/26  Rishabh Dalal                          |
;; |   DESCRIPTION    :  Added 'do and 'assignment in the 'preprocess to    |
;; |                     handle assignment statements.                      |
;; |                     Added 'do? to eval-exp.                            |
;; |                     Wrote 'run-huey-2 to print out hexadecimals instead|
;; |                     of rgb values.                                     |
;;  ------------------------------------------------------------------------

#lang racket
(require "syntax-procs.rkt")
(require "datatypes.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;  ------------------------------------------------------------------------
;;   This code works with the following grammar:
;;
;;
;;    <color> ::= (rgb <byte> <byte> <byte> )
;;               | <varref>
;;               | ( <unary-op> <color> )
;;               | ( <color> <2color-op> <color> )
;;               | ( <color> <1color-op> <number> )
;;               | ( color <var> = <color> in <color> )
;;               | ( do <assignment>* <color> )
;;               | ( define <varref> <= color)   ;;Define
;;               | (<varref> <= color)           ;;Assignment
;; ------------------------------------------------------------------------

;; --------------------------------------------------------------------------
;; This function preprocesses programs from the full language, translating
;; syntactic sugar into core features of the little language.
;; --------------------------------------------------------------------------

(define preprocess
  (lambda (exp)
    (cond ((rgb? exp) exp)
          ((varref? exp)     (make-varref exp))
          
          ((1color? exp)     (make-1color (preprocess (1color->color exp))
                                          (1color->op exp)
                                          (1color->number exp)))

          ((unary? exp)      (if (eq? (unary->op exp) 'darker)  ;;processing 'darker
                                 (make-1color (preprocess (unary->color exp))
                                              '* 0.5)
                                 (make-unary (unary->op exp)
                                             (preprocess (unary->color exp)))))
          ((assignment? exp) (make-assignment (preprocess (assgn->varref exp))
                                              (preprocess (assgn->color exp))))
          ((color-in? exp)   (make-color-in (color-in->var exp)
                                            (preprocess (color-in->val exp))
                                            (preprocess (color-in->body exp))))
          ((do? exp)         (let ((assgn (map preprocess (do->assgn exp)))
                                   (color (preprocess (do->color exp))))
                               (if (null? assgn)
                                   (make-do color)
                                   (make-do-in-fn assgn color))))

          ;; For REPL with top level states

          ((define-var? exp) (make-define (define->varref exp)
                                          (preprocess (define->body exp))))
          ;;2color
          (else             (if (eq? (2color->op exp) 'mix)                    ;;processing 'mix
                                (make-2color (make-1color (preprocess (2color->first-col exp))
                                                          '* 0.5)
                                 '+
                                 (make-1color (preprocess (2color->second-col exp))
                                              '* 0.5))
                                (make-2color (preprocess (2color->first-col exp))
                                             (2color->op exp)
                                             (preprocess (2color->second-col exp))))))))

;; --------------------------------------------------------------------------
;; This function interprets programs from the core language.
;; --------------------------------------------------------------------------

(define white-cell (cell '(rgb 255 255 255)))
(define black-cell (cell '(rgb 0 0 0)))
(define initial-env (bind 'white white-cell
                          (bind 'black black-cell
                                (make-bindings))))

(define eval-exp
  (lambda (exp)
    (if (color? exp)
        ;; outputs an rgb, eg. (rgb 10 10 10)
        ;(eval-helper (preprocess exp) initial-env)

        ;; outputs an hexadecimal, eg. #0a0a0a
        ;(change-hex (eval-helper (preprocess exp) initial-env))

        ;; outputs a list, eg. (10 10 10)
        (post-process (eval-helper (preprocess exp) initial-env))
        (error 'huey "illegal expression -- ~a" exp))))

(define eval-helper
  (lambda (exp env)
    (cond ((rgb? exp)       (eval-rgb exp))
          ((varref? exp)    (eval-varref exp env))  
          ((unary? exp)     (eval-unary exp env))
          ((1color? exp)    (eval-1color exp env))
          ((color-in? exp)  (eval-color-in exp
                                           (bind (color-in->var exp)
                                                 (cell (eval-helper (color-in->val exp)
                                                                    env))
                                                 env)))
          ((2color? exp)    (eval-2color exp env))
          ((do? exp)        (eval-do exp env))

          ;;For REPL with top level variables
          ((assignment? exp) (handle-assign (list exp) env env))
          ((define-var? exp) (eval-define exp env))
            
          ;;checking if the exp in a keyboard
          ((is-keyword? exp) (error
                              'eval-exp "expression is keyword -- ~a" exp))
          (else              (error 'eval-exp "undefined expression -- ~a" exp)))))


;; --------------------------------------------------------------------------
;; Helper functions for eval-exp
;; --------------------------------------------------------------------------

(define eval-rgb
  (lambda (exp)
    (make-rgb (rgb->byte1 exp)
              (rgb->byte2 exp)
              (rgb->byte3 exp))))

(define eval-define
  (lambda (exp env)
    (letrec ((new-cell (cell (eval-helper (define->body exp) env)))
             (curr-env (bind (define->varref exp)
                             new-cell
                             env)))
      (begin 
        (write (post-process (eval-helper (define->varref exp) curr-env)))
        curr-env))))

(define eval-do
  (lambda (exp env)
    (begin (handle-assign (do->assgn exp) env env)  ;;Handling all assignments
           (eval-do-helper (do->color exp) env))))  ;;Evaluating the body

(define handle-assign
  (lambda (assgns env base-env)
    (if (null? assgns)
        env
        (letrec ((assignment (first assgns))
                 (varref     (assgn->varref assignment))
                 (curr       (assoc varref env))
                 (body       (assgn->color assignment)))
          (begin
            (cell-set! (cdr curr) (eval-helper body base-env)) ;;Assignment statement
            (handle-assign (rest assgns) env base-env))))))

(define eval-do-helper                   ;;Evaluates the body of do-assignment statements
  (lambda (exp env)
    (eval-helper exp env)))

(define eval-varref                      ;;Evaluating varref using environment
  (lambda (exp env)
    (let ((curr-cell (assoc exp env)))
      (if curr-cell
          (cell-value (cdr curr-cell))
          (error 'undefined "~a is undefined in the envronment." exp)))))

(define eval-unary                      ;;Evaluating unary exp
  (lambda (exp env)
    (let ((data (eval-helper (unary->color exp) env)))
      (make-rgb (- 255 (rgb->byte1 data))
                (- 255 (rgb->byte2 data))
                (- 255 (rgb->byte3 data))))))

(define eval-color-in
  (lambda (exp env)
    (eval-helper (color-in->body exp) env)))
    
(define eval-1color
  (lambda (exp env)
    (let ((color (eval-helper (1color->color exp) env))
          (number (1color->number exp)))
      (if (eq? (1color->op exp) '*)
          (make-rgb (* number (rgb->byte1 color))
                    (* number (rgb->byte2 color))
                           (* number (rgb->byte3 color)))
          (make-rgb (+ (rgb->byte1 color) number)
                    (+ (rgb->byte2 color) number)
                    (+ (rgb->byte3 color) number))))))

(define eval-2color
  (lambda (exp env)
    (let ((color1 (eval-helper (2color->first-col exp) env))
          (color2 (eval-helper (2color->second-col exp) env)))
      (if (eq? (2color->op exp) '+)
          (make-rgb (+ (rgb->byte1 color1) (rgb->byte1 color2))
                    (+ (rgb->byte2 color1) (rgb->byte2 color2))
                    (+ (rgb->byte3 color1) (rgb->byte3 color2)))
          (make-rgb (- (rgb->byte1 color1) (rgb->byte1 color2))
                    (- (rgb->byte2 color1) (rgb->byte2 color2))
                    (- (rgb->byte3 color1) (rgb->byte3 color2)))))))


;; --------------------------------------------------------------------------
;; This function postprocesses the output of eval-exp to produce required
;; form of the output
;; --------------------------------------------------------------------------

(define post-process        ;;Does:  (rgb 10 10 10) --> (10 10 10)  to output
  (lambda (exp)
    (list (rgb->byte1 exp)
          (rgb->byte2 exp)
          (rgb->byte3 exp))))

;; --------------------------------------------------------------------------
;; run-huey
;; --------------------------------------------------------------------------

(define run-huey
  (lambda ()
    (display "\nhuey! > ")
    (with-handlers ([exn:fail?                     
                     (lambda (e)
                       (displayln
                        "Error. Either input is wrong or the input is not defined."))])
       (write (eval-exp-run-huey (read)))
      (newline))
    (run-huey)
    (newline)))

;; --------------------------------------------------------------------------
;; extra-credit 1
;; Just run (run-huey)
;; --------------------------------------------------------------------------

(define base 16)
(define hex-digits (list 0 1 2 3 4 5 6 7 8 9 "a" "b" "c" "d" "e" "f"))

(define change-hex
  (lambda (rgb)
    (if (rgb? rgb)
        (string-append "#"
                       (make-hex (rgb->byte1 rgb))
                       (make-hex (rgb->byte2 rgb))
                       (make-hex (rgb->byte3 rgb)))
        (error 'not-rgb-value "~a not a valid rgb value."))))

(define make-hex
  (lambda (n)
    (string-append (toString (list-ref hex-digits                 ;;--------> (1)
                                       (truncate (inexact->exact (/ n base)))))
                   (toString (list-ref hex-digits
                                       (truncate (inexact->exact (remainder n base))))))))

(define toString      ;;Converting numbers from (1), if any, to string. 
  (lambda (exp)
    (if (number? exp)
        (number->string exp)
        exp)))

(define eval-exp-run-huey
  (lambda (exp)
    (if (color? exp)
        (change-hex (eval-helper (preprocess exp) initial-env)) ;;Writing hexadecimal form
        (error 'huey "illegal expression -- ~a" exp))))

;; --------------------------------------------------------------------------
;; extra-credit 2
;; Just run (run-huey-2)
;; --------------------------------------------------------------------------

(define run-huey-2
  (let ((en initial-env))
    (lambda ()
      (display "\nhuey! > ")
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (displayln
                          "Error. Either input is wrong or the input is not defined."))])
      (let ((curr-env (eval-exp-with-state (read) en)))
        (begin
          (if (rgb? curr-env)
              (write (post-process curr-env)) ;;Input was varref
              (set! en curr-env)))))          ;;Input was define / assign
      (newline)
       (run-huey-2)
       (newline))))


(define eval-exp-with-state
  (lambda (exp env)
    (if (color? exp)
        (eval-helper (preprocess exp) env)
        (error 'huey "illegal expression -- ~a" exp))))