
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


;; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))


;; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))


;; Problem 5
(define (make-stream f start)
  (lambda ()
    (cons (f start)
          (make-stream f (+ start 1)))))

(define funny-number-stream
  (make-stream (lambda (i) (if (= (remainder i 5) 0)
                               (- 0 i)
                               i))
               1))


;; Problem 6
(define dan-then-dog
  (let ([dd (list "dan.jpg" "dog.jpg")])
    (make-stream (lambda (i) (list-nth-mod dd i))
                 0)))


;; Problem 7
(define (stream-add-zero s)
  (let ([pr (s)])
    (lambda ()
      (cons (cons 0 (car pr))
            (stream-add-zero (cdr pr))))))


;; Problem 8
(define (cycle-lists xs ys)
  (make-stream (lambda (i) (cons (list-nth-mod xs i)
                                 (list-nth-mod ys i)))
               0))


;; Problem 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [iter (lambda (i)
                   (if (>= i len)
                       #f
                       (let ([item (vector-ref vec i)])
                         (if (and (pair? item) (equal? v (car item)))
                             item
                             (iter (+ i 1))))))])
    (iter 0)))


;; Problem 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [next 0])
    (lambda (v)
      (let ([cached (vector-assoc v cache)]) 
        (if cached
            cached
            (let ([result (assoc v xs)])
              (if result 
                  (begin (vector-set! cache next result)
                         (set! next (remainder (+ next 1) n))
                         result)
                  #f)))))))


;; Problem 11 Challenge Problem
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [loop (lambda () (if (< e2 v1)
                                   (loop)
                                   #t))])
       (loop))]))



