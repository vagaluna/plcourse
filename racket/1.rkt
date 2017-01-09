#lang racket

(require "hw/hw5.rkt")

(define s (fun "sum" "n" (ifgreater (var "n") 
                                    (int 0)
                                    (add (var "n")
                                         (call (var "sum") (add (var "n")
                                                                (int -1))))
                                    (int 0))))

(define (make-stream fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f 1))))