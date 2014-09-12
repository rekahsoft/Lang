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

;; File: comparable.rkt
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Sep 11, 2014

(require racket/generic)

;; (provide (contract-out
;;           [bst-make-leaf (-> any bst-node?)]
;;           [bst-insert (-> bst-node? any)]))

;; Define a generic interface for orderable things (thatis things that can be sorted)
(define-generics orderable
  [lt orderable other]
  [lte orderable other]
  [gt orderable other]
  [gte orderable other]
  [eql orderable other]
  #:defaults ([number?
               (define lt <)
               (define lte <=)
               (define gt >)
               (define gte >=)
               (define eql =)]
              [string?
               (define lt string-ci<?)
               (define lte string-ci<=?)
               (define gt string-ci>?)
               (define gte string-ci>=?)
               (define eql equal?)]))
