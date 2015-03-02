#lang lazy

;; Implementation of booleans with only the use of define and lambda

(define my-true (lambda (x)
                  (lambda (y) x)))
(define my-false (lambda (x)
                   (lambda (y) y)))

(define (to-built-in-bool a)
  ((a #t) #f))

(define (not a)
  (lambda (x)
    (lambda (y)
      ((a y) x))))

(define (and-op a b)
  ((a b) a))

(define (my-and . xs)
  (foldr and-op my-true xs))

(define (or-op a b)
  ((a a) b))

(define (my-or . xs)
  (foldr or-op my-false xs))

;;
;; Testing the functions
;;

(displayln "Testing and: should output false then #f on the next line indicating and (as well as and-op) did not evaluate its second argument")
(to-built-in-bool
 (my-and
  (begin (displayln 'false)
         my-false)
  (begin (displayln 'what)
         my-true)))

(newline)

(displayln "Testing or: should output true then #t on the next line indicating or (and or-op) did not evaluate its second argument")
(to-built-in-bool
 (my-or
  (begin (displayln 'true)
         my-true)
  (begin (displayln 'what)
         my-false)))
