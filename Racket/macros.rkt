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

;; File: macros.rkt
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Aug 28, 2014
;; Description: various implementions of macros for learning purposes

(require (for-syntax syntax/parse))

(define-syntax my-while
  (syntax-rules ()
    [(my-while n body ...)
     (if (and (integer? n) (>= n 0))
         (letrec ([f (lambda () body ...)]
                  [g (lambda (i)
                       (cond [(= i 0) (f)]
                             [else (begin
                                     (f)
                                     (g (- i 1)))]))])
           (g n))
         (error"Must be a positive integer"))]))

(define-syntax mylet
  (syntax-rules ()
    [(mylet ([var rhs] ...) body ...) ((lambda (var ...) body ...) rhs ...)]))

(define-syntax (mylet2 stx)
  (syntax-parse stx
    [(_ ((var:id rhs:expr) ...) body ...+) #'((lambda (var ...) body ...) rhs ...)]))
