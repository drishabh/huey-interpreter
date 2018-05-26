;;  ------------------------------------------------------------------------
;; |   FILE           :  datatypes.rkt                                      |
;; |   AUTHOR         :  Rishabh Dalal                                      |
;; |   CREATION DATE  :  2018/03/19                                         |
;; |   DESCRIPTION    :  Implementing the environment.                      |
;;  ------------------------------------------------------------------------

#lang racket
(provide   make-bindings   bind    look-up  var-exists?)

;; --------------------------------------------------------------
;; --       setting up environment                             --
;; --------------------------------------------------------------

(define make-bindings
  (lambda ()
    (list)))

(define bind
  (lambda (var val bindings)
    (cons (cons var val)
          bindings)))

(define look-up
  (lambda (var env)
    (let ((x (assoc var env)))
      (if x
          (cdr x)
          (error 'environment "undefined variable -- ~a" var)))))


(define var-exists?
  (lambda (var env)
    (if (assoc var env)
        #t #f)))