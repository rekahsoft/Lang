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

;; File: avl-tree.rkt
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Sep  2, 2014

(require "comparable.rkt")

;; Structure representing a Binary Search Tree 
(struct bst ())
(struct bst-empty bst () #:transparent)
(struct bst-node bst (val left right) #:transparent)

;; Make a leaf in a binary tree
(define (bst-make-leaf i)
  (bst-node i (bst-empty) (bst-empty)))

;; ----------------------------------------------------------------------

;; First a naive approach to binary search trees
;; Specifically these functions do not maintain any balance
;; of the bst and thus are inefficient in many cases

;; Naive bst insert (not balanced)
(define/match (bst-insert-naive t i)
  [((bst-empty) _) (bst-node i (bst-empty) (bst-empty))]
  [((bst-node v (bst-empty) (bst-empty)) _)
   (if (gte i v)
       (bst-node v (bst-empty) (bst-make-leaf i))
       (bst-node v (bst-make-leaf i) (bst-empty)))]
  [((bst-node v l r) _) #:when (gte i v)
   (bst-node v l (bst-insert-naive r i))]
  [((bst-node v l r) _) #:when (lt i v)
   (bst-node v (bst-insert-naive l i) l)])

;; Naive bst delete (not balanced)
;; (define (bst-delete-naive t i)
;;   (match t
;;     [(bst-empty) t]
;;     [(bst-node v l r) #:when (> i v)
;;      (bst-node v l (bst-delete-naive r i))]
;;     [(bst-node v l r) #:when (lt i v)
;;      (bst-node v (bst-delete-naive l i) r)]
;;     [(bst-node v l r) #:when (= i v)
;;      (match* (l r)
;;              [((bst-empty) (bst-empty)) (bst-empty)]
;;              [((bst-node v1 l1 r1) (bst-empty)) ???]
;;              [((bst-empty) (bst-node v1 l1 r1)) ???]
;;              [((bst-node v1 l1 r1) (bst-node v2 l2 r2)) ???])]))

;; ----------------------------------------------------------------------

;; Below is a more effiecient implementation of BST's; specifically using
;; the AVL binary seach tree algorithm.
;; See: https://en.wikipedia.org/wiki/AVL_tree

;; Calculate AVL score for a particular node
(define/match (avl-score t)
  [((bst-empty)) 0]
  [((bst-node v l r)) (- (bst-height l) (bst-height r))])

;; Rotate a bst 
(define/match (bst-rotate dir t)
  [((quote left-right) _) (bst-rotate 'left (bst-rotate 'right t))]
  [((quote right-left) _) (bst-rotate 'right (bst-rotate 'left t))]
  [((quote left) (bst-node v1 l1 (bst-node v2 l2 r2)))
   (bst-node v2 (bst-node v1 l1 l2) r2)]
  [((quote right) (bst-node v1 (bst-node v2 l2 r2) r1))
   (bst-node v2 l2 (bst-node v1 r2 r1))])

;; Balance AVL binary tree
(define (avl-balance t)
  (let ([score (avl-score t)])
    (match t
      [(bst-node v l r) #:when (= score 2)
       (cond [(< (avl-score l) 0) (bst-rotate 'right-left t)]
             [else (bst-rotate 'right t)])]
      [(bst-node v l r) #:when (= score -2)
       (cond [(> (avl-score r) 0) (bst-rotate 'left-right t)]
             [else (bst-rotate 'left t)])]
      [_ t])))

;; Insert into AVL binary tree
(define (bst-insert t i)
  (match t
    [(bst-empty) (bst-make-leaf i)]
    [(bst-node v l r) #:when (eql i v) t]
    [(bst-node v l r) #:when (gte i v)
     (let* ([r-not (bst-insert r i)]
            [rt (bst-node v l r-not)])
       (avl-balance rt))]
    [(bst-node v l r) #:when (lt i v)
     (let* ([l-not (bst-insert l i)]
            [rt (bst-node v l-not r)])
       (avl-balance rt))]))

;; Delete item from AVL binary tree
(define (bst-delete t i)
  'undefined)

(define (bst-search t i)
  (match t
    [(bst-empty) #f]
    [(bst-node v _ _) #:when (eql i v) #t]
    [(bst-node v _ r) #:when (gt i v) (bst-search r i)]
    [(bst-node v l _) #:when (lt i v) (bst-search l i)]))

(define (bst-flatten t)
  (match t
     [(bst-empty) '()]
     [(bst-node v l r) `(,@(bst-flatten l) ,v ,@(bst-flatten r))]))

(define (bst-height t)
  (match t
    [(bst-empty) 0]
    [(bst-node _ l (bst-empty)) (+ 1 (bst-height l))]
    [(bst-node _ (bst-empty) r) (+ 1 (bst-height r))]
    [(bst-node _ l r) (+ 1 (max (bst-height l) (bst-height r)))]))

(define (list->bst xs)
  (foldr (lambda (x acc)
           (bst-insert acc x))
         (bst-empty) xs))

(define (bst-sort xs)
  (bst-flatten (list->bst xs)))
