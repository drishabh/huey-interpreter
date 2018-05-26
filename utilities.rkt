;;  ------------------------------------------------------------------------
;; |   FILE           :  utilities.rkt                                      |
;; |   AUTHOR         :  Rishabh Dalal                                      |
;; |   CREATION DATE  :  2018/04/25                                         |
;; |   DESCRIPTION    :  Implementing the ADT to store local variables.     |
;;  ------------------------------------------------------------------------

#lang racket
(provide cell cell-value cell-set!)

;; --------------------------------------------------------------------------
;; Cell ADT to store the local variables in.
;; --------------------------------------------------------------------------

(define cell
  (lambda (val)
    (lambda (arg)
      (case arg
        ('value val)
        ('set
         (lambda (new-val)
           (set! val new-val)
           new-val))
        (else
         (error "Unknown prodcedure on the cell."))))))

;; Getting value of a cell
(define cell-value
  (lambda (cell)
    (cell 'value)))

;; Changing value of a cell
(define cell-set!
  (lambda (cell new-val)
    ((cell 'set) new-val)))
