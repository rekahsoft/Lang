#lang racket/gui

;; (C) Copyright Collin Doering 2011
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

;; File: gui-repl.rkt
;; Author: Collin J. Doering <rekahsoft@gmail.com>
;; Date: Jul  9, 2011

(define-namespace-anchor a)

(define frame (new frame% [label "Test REPL"]))

(define text-field (new text-field%
			[label "REPL: "]
			[parent frame]
			[callback (lambda (i e)
				    (if (equal? 'text-field-enter (send e get-event-type))
					(let* ([cur-input (send i get-value)]
					       [ns (namespace-anchor->namespace a)]
					       [eval-val (with-handlers ([exn:fail? (lambda (exn) 'repl-error)])
							   (eval (read (open-input-string cur-input)) ns))]
					       [repl-outp (open-output-string)])
					  (if (equal? eval-val 'repl-error)
					      (fprintf repl-outp "Error evalulting \"~a\"" cur-input)
					      (fprintf repl-outp "~a :evals-to: ~a" cur-input eval-val))
					  (send repl-msg set-label (get-output-string repl-outp))
					  (send i set-value ""))
					#f))]))

(define bottom-panel (new horizontal-panel%
			  [parent frame]
			  [alignment '(left center)]))

(define repl-msg (new message%
		      [label "Welcome to the REPL"]
		      [parent bottom-panel]
		      [auto-resize #t]))

(define menu-bar (new menu-bar% [parent frame]))

(define file-menu (new menu%
		       [label "File"]
		       [parent menu-bar]))

(define quit-menu-item (new menu-item%
			    [label "Quit"]
			    [parent file-menu]
			    [callback (lambda (i e)
					(let* ([quit-dialog (new dialog% [label "Really Quit?"])]
					       [quit-msg (new message%
							      [label "Are you sure you want to quit?"]
							      [parent quit-dialog])]
					       [button-panel (new horizontal-panel%
								  [parent quit-dialog]
								  [alignment '(center center)])]
					       [ok-button (new button%
							       [label "OK"]
							       [parent button-panel]
							       [callback (lambda (i e)
									   (exit 0))])]
					       [cancel-button (new button%
								   [label "Cancel"]
								   [parent button-panel]
								   [callback (lambda (i e)
									       (send quit-dialog show #f))])])
					  (send quit-dialog show #t)))]))

;; Display the main frame
(send frame show #t)
