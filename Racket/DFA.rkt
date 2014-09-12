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

  (define-syntax-class state
    #:description "dfa state"
    (pattern (name:id (~optional (~seq (~and #:end end?))) trans:transition ...+)
             #:with (in ...) #'(trans.in ...)
             #:with (out ...) #'(trans.out ...)))
  ;; (pattern (name:id (~seq (~and #:end end2?)) (~optional (~and #:end end?))))
  
  (syntax-parse stx
    [(_ name:id alpha:expr start:state rests:state ...)
     #:fail-when (check-duplicate-identifier
                  (syntax->list
                   #'(start.name rests.name ...)))
                 "duplicate state names"
     #'(define name
         (letrec ([start.name
                   (dfaStartState
                    (match-lambda [start.in start.out] ...))]
                  [rests.name
                   (dfaState
                    (match-lambda [rests.in rests.out] ...))] ...)
           (dfa 'alpha start.name '(start.name rests.name ...))))]))

;; ----------------------------------------------------------------------------

(define-dfa odd-binary (0 1)
  [s0 (0 -> s0)
      (1 -> s1)]
  [s1 (0 -> s0)
      (1 -> s1)]
  [s2 #:end
      (0 -> s2)
      (1 -> s1)]
  [s3 #:dead (0 -> s3) (1 -> s3)])

;; Odd binary dfa expansion
(define odd-dfa
  (letrec ([s0 (dfaStartState (match-lambda [0 s0]
                                            [1 s1]))]
           [s1 (dfaEndState (match-lambda [0 s0]
                                          [1 s1]))])
    (dfa '(0 1) s0 '(s0 s1))))

;; Even binary dfa expansion
(define even-dfa
  (letrec ([s0 (dfaStartState (match-lambda [0 s0]
                                               [1 s1]))]
           [s1 (dfaState (match-lambda [0 s2]
                                       [1 s1]))]
           [s2 (dfaEndState (match-lambda [0 s0]
                                          [1 s1]))])
    (dfa '(0 1) s0 '(s0 s1 s2))))

;; Odd binary dfa macro
;; (define-dfa odd-dfa (0 1)
;;      [s0 (0 -> s0)
;;          (1 -> s1)]
;;      [s1 end
;;          (0 -> s0)
;;          (1 -> s1)])

;; ;; Even binary dfa macro
;; (define-dfa even-dfa (0 1)
;;      [s0 (0 -> s0)
;;          (1 -> s1)]
;;      [s1 (0 -> s2)
;;          (1 -> s1)]
;;      [s2 end
;;          (0 -> s0)
;;          (1 -> s1)])
