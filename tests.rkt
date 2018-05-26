;;  ------------------------------------------------------------------------
;; |   FILE           :  tests.rkt                                          |
;; |   AUTHOR         :  Rishabh Dalal                                      |
;; |   CREATION DATE  :  2018/04/20                                         |
;; |   DESCRIPTION    :  Test cases for hw10.                               |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2018/04/226  Rishabh Dalal                         |
;; |   DESCRIPTION    :  Added more tests.                                  |
;;  ------------------------------------------------------------------------

#lang racket

(require rackunit)
(require "syntax-procs.rkt")
(require "interpreter.rkt")

;; ------------------------------------------------------------------------
;; preprocessor
;; ------------------------------------------------------------------------

(check-equal? (preprocess '(invert (rgb 150 99 42)))
              '(invert (rgb 150 99 42)))
(check-equal? (preprocess '(darker (rgb 150 99 42)))
              '((rgb 150 99 42) * 0.5))
(check-equal? (preprocess '((rgb 4 4 4) +
                           ((rgb 150 99 42) mix (rgb 50 108 21))))
              '((rgb 4 4 4) +
               (((rgb 150 99 42) * 0.5) + ((rgb 50 108 21) * 0.5))))

(check-exn exn:fail? (lambda ()
                       (preprocess '((rgb 1 2 3) + 3)))
                     "shift not +")

(check-equal? (preprocess 'white)
              'white)

(check-equal? (preprocess '((rgb 255 0 255) mix (rgb 0 255 0)))
              '(((rgb 255 0 255) * 0.5) + ((rgb 0 255 0) * 0.5)))

(check-equal? (preprocess '(color purple = ((rgb 255 0 0) mix (rgb 0 0 255))
                                  in (darker purple)))
              '(color purple = (((rgb 255 0 0) * 0.5) + ((rgb 0 0 255) * 0.5))
                      in (purple * 0.5)))

(check-equal? (preprocess '(do (rgb 255 0 0)))
              '(do (rgb 255 0 0)))

(check-equal? (preprocess '(color c = (rgb 0 255 0) in
                                  (do (c <= (c mix (rgb 0 0 255)))
                                    (c <= (invert c))
                                    (darker (c shift 5)))))
              '(color c = (rgb 0 255 0) in
                      (do (c <= ((c * 0.5) + ((rgb 0 0 255) * 0.5)))
                        (c <= (invert c))
                        ((c shift 5) * 0.5))))

;; ------------------------------------------------------------------------
;; evaluator
;; ------------------------------------------------------------------------

(check-equal? (eval-exp '(invert (rgb 150 99 42)))
              '(105 156 213))
(check-equal? (eval-exp '((rgb 150 99 42) + (rgb 50 18 241)))
              '(200 117 255))
(check-equal? (eval-exp '((rgb 255 0 255) mix ((rgb 0 255 0) + (rgb 4 4 4))))
              '(129 127 129))

(check-exn exn:fail? (lambda ()
                       (eval-exp '((rgb 0 0 0) + 3)))
                     "shift not +")
(check-equal? (eval-exp 'white)
              '(255 255 255))

(check-equal? (eval-exp '((rgb 255 0 255) mix (rgb 0 255 0)))
              '(127 127 127))

(check-equal? (eval-exp '(color pink = (white mix (rgb 255 0 0))
                                in (darker pink)))
              '(127 63 63))

(check-exn exn:fail? (lambda ()
                       (eval-exp 'gray))
                     "shift not +")

(check-equal? (eval-exp 'white)
              '(255 255 255))

(check-equal? (eval-exp '(do (rgb 255 0 0)))
              '(255 0 0))

(check-equal? (eval-exp '(color c = (rgb 0 255 0) in
                                (do (c <= (c mix (rgb 0 0 255)))
                                  (c <= (invert c))
                                  (darker (c shift 5)))))
              '(127 66 66))

(check-equal? (eval-exp '(color c = (rgb 0 255 0) in
                                (color d = (rgb 0 0 255) in
                                       (do (c <= (c mix d))
                                         (d <= (c mix d))
                                         ((c mix d) shift 5)))))
                        '(5 99 163))

;; ------------------------------------------------------------------------
;;    rgb to hex
;; ------------------------------------------------------------------------

(check-equal? (change-hex '(rgb 255 255 255)) "#ffffff")
(check-equal? (change-hex '(rgb 10 10 10)) "#0a0a0a")

;; --------------------------------------------------------------------------