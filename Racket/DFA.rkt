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

;; A structure to represent a dfa state
(struct dfaState (name trans end? dead?))

;; A structure representing a dfa
(struct dfa (alpha start states))

;; A structure to represent a nfa state
;; TODO

;; A structure representing a nfa
;; TODO

;; Given a dfa and a list of inputs returns 'accept when the dfa completes in a end success
;; state and 'reject otherwise
(define (compute-dfa m xs)
  (define/match (run-dfa state ys)
    [((dfaState _ _ #t _) '()) 'accept]
    [(_ '()) 'reject]
    [((dfaState _ _ #f #t) _) 'reject]
    [((dfaState _ f _ _) (cons z zs)) (run-dfa (f z) zs)])
  (run-dfa (dfa-start m) xs))

;; Macro for defining dfa structures in a syntactically clean way
(define-syntax (define-dfa stx)
  (define-syntax-class transition
    #:description "dfa state transition"
    (pattern (in (~optional ->) out:id)))

  (define-splicing-syntax-class state
    #:description "dfa state"
;;    (pattern (name:id (~or (~optional (~and #:dead deader?))
;;                           (~optional (~and #:end ender?))) ...)
;;             #:with dead? #'(if deader? #'#t #'#f)
;;             #:with end? #'(if ender? #'#t #'#f)
;;             #:with (in ...) #'(_)
;;             #:with (out ...) #'(name))
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
     #`(define name
         (letrec ([start.name
                   (dfaState 'start.name
                    (match-lambda [start.in start.out] ...)
                    start.end?
                    #f ;start.dead?
                    )]
                  [rests.name
                   (dfaState 'rests.name
                    (match-lambda [rests.in rests.out] ...)
                    rests.end?
                    #f ;rests.dead?
                    )] ...)
           (dfa alpha start.name `(,start.name ,rests.name ...))))]))

;; TODO: implement conversion of nfa to dfa using the powerset construction
(define (nfa->dfa n)
  'undefined)

(define (dfa-reverse d)
  (letrec ([states (dfa-states d)]
           [end-states (map dfaState-name (filter dfaState-end? states))]
           [dfa-trans-tbl (foldr (lambda (s acc)
                                   (hash-set acc
                                             (dfaState-name s)
                                             (foldr (lambda (i a)
                                                      (hash-set a i (dfaState-name ((dfaState-trans s) i))))
                                                    (hash)
                                                    (dfa-alpha d))))
                                 (hash)
                                 (dfa-states d))]
           [gen-nfa-trans-tbl (lambda (seen tbl)
                                (for ([(s h) (in-hash dfa-trans-tbl)])
                                  (for ([(a t) (in-hash h)])
                                    (set! tbl (hash-set tbl t (hash-set (hash-ref tbl t) a
                                                               (set-add (hash-ref (hash-ref tbl t) a) s))))
                                    (when (not (equal? s t))
                                      (set! seen (set-add seen s)))))
                                (for ([s (in-set (set-subtract (list->set (map dfaState-name states)) seen))])
                                  (set! tbl (hash-remove tbl s)))

                                tbl)]
           [nfa-trans-tbl->nfa (lambda (tbl)
                                 tbl)])
    (nfa-trans-tbl->nfa
     (gen-nfa-trans-tbl
      (list->set end-states)
      (let* ([o (foldr (lambda (i acc)
                         (hash-set acc i (set))) (hash) (dfa-alpha d))]
             [h (foldr (lambda (s acc)
                         (hash-set acc (dfaState-name s) o)) (hash) states)])
       (if (<= (length end-states) 1)
           h
           (hash-set h (gensym) (hash 'epsilon (list->set end-states)))))))))

;; TODO
;; (list->vector (dfa-states d))
;; (define (dfa-reverse d)
;;   (letrec ([alpha (dfa-alpha d)]
;;            [start (dfa-start d)]
;;            [ends (filter dfaState-end? (dfa-states d))]
;;            [s0 (nfaState (match-lambda (['epsilon ends])) #f)]
;;            [dfa-rev (match-lambda* (s n)
;;                        [((dfaState f e d) n-prime) 'undefined])])
;;     (nfa alpha s0 (dfa-rev ends nnnnnn))
;;     (dfa-rev start nnnnn)))

(define (minimize-dfa d)
  (nfa->dfa (dfa-reverse (nfa->dfa (dfa-reverse d)))))

;; Todo:
;;  - error when 'in' pattern in the transition syntax class is not an element of alpha
;;  - error when given no end state
;;  - add keyword #:dead to create a dead state: (define-dfa n (0 1) (s0 #:dead))
;;  - check to ensure all transitions goto a valid state name; fail otherwise
;;  - check to ensure all transitions 'come from' a valid input; fail otherwise
;; ----------------------------------------------------------------------------

;; Odd binary dfa expansion
(define odd-dfa-expansion
  (letrec ([s0 (dfaState 's0
                (match-lambda [0 s2]
                              [1 s1])
                #f #f)]
           [s1 (dfaState 's1
                (match-lambda [0 s2]
                              [1 s1])
                #t #f)]
           [s2 (dfaState 's2
                (match-lambda [0 s2]
                              [1 s1])
                #f #f)])
    (dfa '(0 1) s0 '(s0 s1 s2))))

;; Even binary dfa expansion
(define even-dfa-expansion
  (letrec ([s0 (dfaState 's0
                (match-lambda [0 s1]
                              [1 s2])
                #f #f)]
           [s1 (dfaState 's1
                (match-lambda [0 s1]
                              [1 s2])
                #t #f)]
           [s2 (dfaState 's2
                (match-lambda [0 s1]
                              [1 s2])
                #f #f)])
    (dfa '(0 1) s0 '(s0 s1 s2))))

;; Even binary DFA
(define-dfa even-dfa '(0 1)
  [s0 (0 -> s1)
      (1 -> s2)]
  [s1 #:end
      (0 -> s1)
      (1 -> s2)]
  [s2 (0 -> s1)
      (1 -> s2)])

;; Odd binary DFA
(define-dfa odd-dfa '(0 1)
  [s0 (0 -> s2)
      (1 -> s1)]
  [s1 #:end
      (0 -> s2)
      (1 -> s1)]
  [s2 (0 -> s2)
      (1 -> s1)])

(define-dfa divisible-by-four-dfa '(0 1)
  [s0 (0 -> s3)
      (1 -> s1)]
  [s1 (0 -> s2)
      (1 -> s1)]
  [s2 (0 -> s3)
      (1 -> s1)]
  [s3 #:end
      (0 -> s3)
      (1 -> s1)])

;; ----------------------------------------------------------------------------
;; This section shows two features that would be nice to have added to the define-dfa macro
;; Specifically:
;;  - Add transition to dead state using #:dead keyword; dfa transition of form (in:id (~optional ->) #:dead)

(define-dfa text-file-dfa (string->list "AaBbCcDdEeFfGgHhJjLlMmNnOoPpQqRrSsTtUuVvWwRrXxYyZz1234567890-=\\`!@#$%^&*()_+|~[];',./{}:\"<>?")
  [s0 (#\. -> s1)
      (_   -> s0)]
  [s1 (#\t -> s2)
      (#\. -> s1)
      (_   -> s0)]
  [s2 (#\x -> s3)
      (_   -> s0)]
  [s3 (#\t -> s4)
      (#\. -> s1)
      (_   -> s0)]
  [s4 #:end
      (#\. -> s1)
      (_   -> s0)])

;; (define-dfa text-file-dfa (#\A #\a #\B #\b #\C #\c #\D #\d #\E #\e #\F #\f #\G #\g #\H #\h #\I #\i #\J #\j #\K #\k #\L #\l #\M #\m #\N #\n #\O #\o #\P #\p #\Q #\q #\R #\r #\S #\s #\T #\t #\U #\u #\V #\v #\W #\w #\X #\x #\Y #\y #\Z #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
;;   [s0 (#\. -> s1)
;;       (_   -> s0)]
;;   [s1 (#\t -> s2)
;;       (_   -> #:dead)]
;;   [s2 (#\x -> s3)
;;       (_   -> #:dead)]
;;   [s3 (#\t -> s4)
;;       (_   -> #:dead)]
;;   [s4 #:end
;;       (_   -> #:dead)])

;; (define-dfa empty-dfa ()
;;   [s0 #:end
;;         (_ -> dead)]
;;   [dead (_ -> dead)])

;; (define-dfa empty-dfa ()
;;   [s0 #:end (_ -> #:dead)])

;; (define-dfa empty-dfa ()
;;   [s0 #:dead #:end])

;;(define-dfa binary-empty-dfa (0 1)
;;  [s0 #:dead #:end])

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
