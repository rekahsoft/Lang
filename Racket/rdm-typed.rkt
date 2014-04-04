#lang typed/racket

;; (C) Copyright Collin Doering 2013
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

;; File: rdm-typed.rkt
;; Author: Collin J. Doering <rekahsoft@gmail.com>
;; Date: Jul  2, 2013

(: factorial : Integer -> Integer)
(define (factorial n)
  (: factorial-helper :  Integer Integer -> Integer)
  (define (factorial-helper n acc)
            (cond [(<= n 1) acc]
                  [else (factorial-helper (- n 1) (* acc n))]))
  (factorial-helper n 1))
