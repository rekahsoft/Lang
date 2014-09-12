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

;; File: simple-sorts.rkt
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Sep 11, 2014

;; Insert a element into a sorted list
(define (insert i lst)
  (cond [(empty? lst) (list i)]
        [(>= i (first lst)) (cons (first lst) (insert i (rest lst)))]
        [(< i (first lst)) (cons i lst)]))

;; Implementation of insertion sort
(define (insert-sort lst)
  (define (isort xs acc)
    (cond [(empty? xs) acc]
          [else (isort (rest xs) (insert (first xs) acc))]))
  (isort lst '()))

;; Implementation of selection sort using immutable data
(define (selection-sort xs)
  (define (selsort xs acc)
    (cond [(empty? xs) acc]
          [else (selsort (remove (apply min xs) xs) (cons (apply min xs) acc))]))
  (reverse (selsort xs '())))

;; Merge two lists such that order is maintained
(define (merge xs ys)
  (cond [(empty? xs) ys]
	[(empty? ys) xs]
	[(< (first xs) (first ys)) (cons (first xs) (merge (rest xs) ys))]
	[(equal? (first xs) (first ys))
	   (cons (first xs) (cons (first ys) (merge (rest xs) (rest ys))))]
	[else (cons (first ys) (merge xs (rest ys)))]))

;; The merge function re-written using pattern matching instead of cond
(define/match (merge as bs)
  [('() _) bs]
  [(_ '()) as]
  [((cons x xs) (cons y _)) #:when (< x y) (cons x (merge xs bs))]
  [((cons x xs) (cons y ys)) #:when (= x y) (cons x (cons y (merge xs ys)))]
  [((cons x _) (cons y ys)) #:when (> x y) (cons y (merge as ys))])

;; Implementation of merge sort
(define (merge-sort xs)
  (cond [(empty? xs) empty]
	[(empty? (rest xs)) xs]
	[else (merge (merge-sort (take xs (quotient (length xs) 2)))
		     (merge-sort (drop xs (quotient (length xs) 2))))]))
