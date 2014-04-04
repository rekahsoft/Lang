#lang racket

;; (C) Copyright Collin Doering 2012
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

;; File: post-fix-expressions.rkt
;; Author: Collin J. Doering <rekahsoft@gmail.com>
;; Date: Jul  8, 2012

(define (parse-expr-list xs)
  (cond [(empty? xs) (printf "~n")]

	[else (printf "~a~n" (eval-expr (string->list (first xs))))
	      (parse-expr-list (rest xs))]))

(define (eval-expr xs)
  (define (eval-expr-H xs ys)
    (cond [(and (empty? xs) (empty? ys)) (error 'invalid-expr)]
	  [(and (empty? xs) (equal (length ys) 1)) (first ys)]
	  [(number? (first xs)) (eval-expr-H (rest xs) (cons (char->integer (first xs)) ys))]
	  [(char? )])))

(define str-tb-eval(make-parameter '()))

(define runtime-options
  (command-line
   #:program "Postfix expression evaluator"
   #:once-any
   [("-s" "--string") str "Pass in one or more strings to be evaluated"
                          (str-tb-eval (cons (str-tb-eval)))]))

(parse-command-line "postfix-exprs" (current-command-line-arguments) runtime-options)

(if (empty? (str-tb-eval))
    (parse-expr-list (str-tb-eval))
    (eval-expr-interactive))
