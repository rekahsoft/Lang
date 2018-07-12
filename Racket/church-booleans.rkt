#lang racket

;; (C) Copyright Collin J. Doering 2014
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; File: church-booleans.rkt
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Oct 28, 2014

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
  (let ([evaled-a (a)])
    ((evaled-a (b)) evaled-a)))

(define (my-and . xs)
  (foldr and-op my-true xs))

(define (or-op a b)
  (let ([evaled-a (a)])
    ((evaled-a evaled-a) (b))))

(define (my-or . xs)
  (foldr or-op my-false xs))

;; (lazy-fun (f x1 ... xn) body) => macro with name "f-lazy" generated which is like the lazy-funcall macro
;; (lazy-funcall f x1 ... xn) ==> (f (lambda () x1) ... (lambda () xn))

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
