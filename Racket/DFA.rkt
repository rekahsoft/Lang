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

;; File: DFA.rkt
;; Author: Collin J. Doering <collin.doering@rekahsoft.ca>
;; Date: Aug 27, 2014
;; Description: an implementation of Determinalistic Finite Autamata

(require (for-syntax syntax/parse))

;; Some structures to represent a DFA
(struct dfaState (trans))
(struct dfaEndState dfaState ())
(struct dfaStartState dfaState ())
(struct dfaStartEndState dfaState ())

(struct dfa (alpha start states))

(define (compute-dfa m xs)
  (define/match (run-dfa state ys)
    [((or (dfaEndState _)
          (dfaStartEndState _)) '()) 'accept]
    [(_ '()) 'reject]
    [((dfaState f) (cons z zs)) (run-dfa (f z) zs)])
  (run-dfa (dfa-start m) xs))

(define-syntax (define-dfa stx)
  (define-syntax-class transition
    #:description "dfa state transition"
    (pattern (in (~optional ->) out:id)))

  (define-splicing-syntax-class state
    #:description "dfa state"
    (pattern (name:id #:end trans:transition ...+)
             #:with end? #'#t
             #:with (in ...) #'(trans.in ...)
             #:with (out ...) #'(trans.out ...))
    (pattern (name:id trans:transition ...+)
             #:with end? #'#f
             #:with (in ...) #'(trans.in ...)
             #:with (out ...) #'(trans.out ...)))
  
  (syntax-parse stx
    [(_ name:id alpha:expr start:state rests:state ...)
     #:fail-when (check-duplicate-identifier
                  (syntax->list
                   #'(start.name rests.name ...)))
                 "duplicate state names"
     (with-syntax ([startSt (if (syntax->datum (attribute start.end?)) #'dfaStartEndState #'dfaStartState)]
                   [restsSt (map (lambda (x) (if (syntax->datum x) #'dfaEndState #'dfaState)) (attribute rests.end?))])
     #'(define name
         (letrec ([start.name
                   (startSt
                    (match-lambda [start.in start.out] ...))]
                  [rests.name
                   ((if rests.end? dfaEndState dfaState) ;restsSt
                    (match-lambda [rests.in rests.out] ...))] ...)
           (dfa 'alpha start.name '(start.name rests.name ...)))))]))

;; Todo:
;;  - error when 'in' pattern in the transition syntax class is not an element of alpha
;;  - error when given no end state
;;  - add keyword #:dead to create a dead state: (define-dfa n (0 1) (s0 #:dead))
;;  - check to ensure all transitions goto a valid state name; fail otherwise
;;  - check to ensure all transitions 'come from' a valid input; fail otherwise
;; ----------------------------------------------------------------------------

;; Odd binary dfa expansion
;(define odd-dfa
;  (letrec ([s0 (dfaStartState (match-lambda [0 s0]
;                                            [1 s1]))]
;           [s1 (dfaEndState (match-lambda [0 s0]
;                                          [1 s1]))])
;    (dfa '(0 1) s0 '(s0 s1))))

;; Even binary dfa expansion
;(define even-dfa
;  (letrec ([s0 (dfaStartEndState (match-lambda [0 s0]
;                                               [1 s1]))]
;           [s1 (dfaState (match-lambda [0 s0]
;                                       [1 s1]))])
;    (dfa '(0 1) s0 '(s0 s1 s2))))

;; Even binary DFA
(define-dfa even-dfa (0 1)
  [s0 (0 -> s1)
      (1 -> s2)]
  [s1 #:end
      (0 -> s1)
      (1 -> s2)]
  [s2 (0 -> s1)
      (1 -> s2)])

;; Odd binary DFA
(define-dfa odd-dfa (0 1)
  [s0 (0 -> s2)
      (1 -> s1)]
  [s1 #:end
      (0 -> s2)
      (1 -> s1)]
  [s2 (0 -> s2)
      (1 -> s1)])

;; Only epsilon (empty) DFA
;; (define (empty-dfa name xs)
;;   (define-dfa name xs
;;     [s0 #:end
;;         (_ -> s1)]
;;     [s1 #:dead]))

;; ----------------------------------------------------------------------------
;; Some simple tests

(define (integer->binary-list n)
  (define (ibl n acc)
    (cond [(zero? n) acc]
          [else (ibl (quotient n 2) (cons (modulo n 2) acc))]))
  (if (zero? n) '(0) (ibl n '())))

(for ([i 1000])
  (displayln (compute-dfa even-dfa (integer->binary-list i)))
  (displayln (compute-dfa odd-dfa  (integer->binary-list i)))
  (newline))
