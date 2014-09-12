#lang racket

;; File: rdm.rkt
;; Date: Oct 25, 2010
;; Author: Collin J. Doering <rekahsoft@gmail.com
;; Description: Random source file to experiment while learning racket (plt scheme)

;; factorial int[>=0] -> int[>=0]
;; Purpose: returns the factorial of the given positive (or zero) integer
;; Examples/Tests:

(define (factorial n)
  (define (factorial-helper n acc)
            (cond [(<= n 1) acc]
                  [else (factorial-helper (- n 1) (* acc n))]))
  (if (integer? n)
      (factorial-helper n 1)
      (error "Expects argument to be an integer!")))

;; factorial function written using pattern matching
(define (factorial1 n)
  (define/match (fac i acc)
      [(0 _) acc]
      [(n _) (fac (- n 1) (* n acc))])
  (fac n 1))

(define factorial!
  (letrec ([fact-helper (lambda (n acc)
			  (if (<= n 1) acc (fact-helper (- n 1) (* acc n))))]
	   [fact! (lambda (n)
		    (fact-helper n 1))])
    fact!))

(define (factorial-close [n 0])
  (letrec ([acc 1]
	   [x 1]
	   [fac-c (lambda ()
		    (cond [(> x n) (set! n (+ n 1)) acc]
			  [else (set! acc (* x acc))
				(set! x (+ x 1))
				(fac-c)]))])
    fac-c))

(define (sum-digits n [base 10])
  (letrec ([sum-digits-helper
	    (lambda (n acc)
	      (cond [(zero? (floor (/ n base))) (+ acc (remainder n base))]
		    [else (sum-digits-helper (floor (/ n base)) (+ acc (remainder n base)))]))])
    (sum-digits-helper n 0)))

;; fibinocci sequences
;; Very slow...big-O anaylsis of O(2^n) (not 100% sure tho)
(define (fib n)
  (cond [(<= n 0) 0]
	[(= n 1) 1]
	[else (+ (fib (- n 1)) (fib (- n 2)))]))

;; fibinocci sequence...but implemented smart ;) haven't looked at the big-O analysis yet
(define (fast-fib n)
  (letrec ([fib-lst empty]
	   [gen-fib (lambda (n x)
		      (cond [(> x n) (first fib-lst)]
			    [(= x 0) (set! fib-lst (cons 0 empty))
			     (gen-fib n (+ x 1))]
			    [(= x 1) (set! fib-lst (cons 1 fib-lst))
			     (gen-fib n (+ x 1))]
			    [else (let ([fibx (+ (first fib-lst) (second fib-lst))])
				    (set! fib-lst (cons fibx fib-lst))
				    (gen-fib n (+ x 1)))]))])
    (gen-fib n 0)))

;; another fibinocci sequence function but with significantly improved memory performance :D (TODO: big-O analysis)
(define (fast-mem-fib n)
  (letrec ([fib-dot-lst empty]
	   [gen-fib (lambda (n x)
		      (cond [(> x n) (car fib-dot-lst)]
			    [(= x 0) (set! fib-dot-lst (cons 0 empty))
			             (gen-fib n (+ x 1))]
			    [(= x 1) (set! fib-dot-lst (cons 1 0))
			             (gen-fib n (+ x 1))]
			    [else (let* ([fst (car fib-dot-lst)]
					 [scd (cdr fib-dot-lst)]
					 [fibx (+ fst scd)])
				    (set! fib-dot-lst (cons fibx fst))
				    (gen-fib n (+ x 1)))]))])
    (gen-fib n 0)))

;; fibinocci closure..pretty much the same as fast-mem-fib but returns a gen-fib like function that takes
;; no paramters but instead encapsulates the values for n and x thus creating a fibinocci closure starting at n
(define (fibc [n 0])
  (letrec ([fib-dot-lst empty]
	   [x 0]
	   [gen-fib-c (lambda ()
			(cond [(> x n) (set! n (+ n 1))
			               (car fib-dot-lst)]
			      [(= x 0) (set! fib-dot-lst (cons 0 empty))
			               (set! x (+ x 1))
			               (gen-fib-c)]
			      [(= x 1) (set! fib-dot-lst (cons 1 0))
			               (set! x (+ x 1))
			               (gen-fib-c)]
			      [else (let* ([fst (car fib-dot-lst)]
					   [scd (cdr fib-dot-lst)]
					   [fibx (+ fst scd)])
				               (set! fib-dot-lst (cons fibx fst))
					       (set! x (+ x 1))
				               (gen-fib-c))]))])
    gen-fib-c))

;; pow num num -> num
;; Purpose: given two real numbers x and n returns x^n
;; Examples/Tests:

(define (pow x n)
  (define (pow-helper x n acc)
    (cond [(= n 0) acc]
          [(> n 0) (pow-helper x (- n 1) (* acc x))]
          [(< n 0) (pow-helper x (+ n 1) (* acc (/ 1 x)))]))
  (pow-helper x n 1))

;; Expandtion of the below macro:
;; (define (natural-number? n)
;;   (if (and (interger? n) (>= n 0) #t #f)))

(define natural-number?
  (lambda (n)
    (if (and (integer? n) (>= n 0)) #t #f)))

(define average-num
  (lambda lst
    (/ (apply + lst) (length lst))))

(define (average-list lst)
  (define (sum-list lst acc)
    (cond [(empty? lst) acc]
          [else (sum-list (rest lst) (+ acc (first lst)))]))
  (/ (sum-list lst 0) (length lst)))

;; increasing common interval
(define (icd-interval i j d)
  (define (icd-interval-helper i j d acc)
    (cond [(> i j) acc]
          [else (icd-interval-helper (+ i d) j d (cons i acc))]))
  (if (> i j)
      (error "i > j for a increasing common interval list to be generated!")
      (reverse (icd-interval-helper i j d empty))))

;; interval num num -> listof(num)
;; Purpose: Given two 
(define (interval i j)
  (define (interval-helper i j acc)
    (cond [(> i j) acc]
          [else (interval-helper (+ i 1) j (cons i acc))]))
  (reverse (interval-helper i j empty)))

(define (repeat f n)
  (define (rep i acc)
    (cond [(<= i 0) acc]
          [else (rep (- i 1) (cons (f) acc))]))
  (rep n '()))

;; common poduct interval
(define (cp-interval i j m)
  (map (lambda (x) (if (= x 0) x (* m x))) (interval i j)))

;; letrec is cool :P
;; (letrec [(fact! (lambda (n) (if (<= n 1) 1 (* n (fact! (- n 1))))))]
;;    (fact! 5))

;; take a looksi at racket/tcp and racket/ssl

(define (client)
  (let-values ([(s-in s-out) (tcp-connect "localhost" 1342)])
    (let ([read-and-display
           (lambda (in-port)
             (let ([responce (read in-port)])
               (display responce)
               (newline)))])
      (read-and-display s-in)
      (write (read-line (current-input-port) 'return-linefeed) s-out)
      (close-output-port s-out)
      (read-and-display s-in)
      (close-input-port s-in))))

;; server
(define listener (tcp-listen 1342))
(let echo-server ()
  (define-values (in out) (tcp-accept listener))
  (thread (lambda ()
            (copy-port in out)
            (close-output-port out)))
  (echo-server))

;; server (Version 2)
(define listener (tcp-listen 1342))
(define (server)
  (let-values ([(in out) (tcp-accept listener)])
    (thread (lambda ()
              (copy-port in out)
              (close-output-port out))))
  (server))

(define (read-it-all f-in [acc ""])
	   (let ([line (read-line f-in)])
	     (if (eof-object? line) (begin acc (close-input-port f-in)) (read-it-all f-in (string-append acc line "\n")))))

;; takes a lowercase char and returns it shifted by 13 characters
(define (rot-char char)
	   (cond [(or (char-symbolic? char) (char-numeric? char) (char-whitespace? char)) char]
		 [(< (char->integer char) 109) (integer->char (modulo (+ (char->integer char) 13) 122))]
		 [else (integer->char (+ 96 (modulo (+ (char->integer char) 13) 122)))]))

(define (rot13 str)
  (letrec ([rot13-helper (lambda (lst acc)
			   (cond [(empty? lst) acc]
				 [(char-upper-case? (first lst)) (rot13-helper (rest lst) (cons (char-upcase (rot-char (char-downcase (first lst)))) acc))]
				 [else (rot13-helper (rest lst) (cons (rot-char (first lst)) acc))]))])
    (list->string (reverse (rot13-helper (string->list str) empty)))))

;; a much better written rot13 which takes advantage of testing intervals
(define (best-rot13 str)
  (letrec
      ;; add-to-char char int -> char
      ;; Purpose: takes the unicode value of the given char and adds n evauluating to the char the additions represents
      ([add-to-char (lambda (char n)
		      (integer->char (+ n (char->integer char))))]
       ;; best-rot listof(char) (or listof(char) acc) -> listof(char)
       ;; Purpose: Given a list of characters returns the rot13 representation
       [best-rot
	(lambda (lst acc)
	  (cond [(empty? lst) acc]
		[(<= 65 (char->integer (first lst)) 77) (best-rot (rest lst) (cons (add-to-char (first lst) 13) acc))]
		[(<= 78 (char->integer (first lst)) 90) (best-rot (rest lst) (cons (add-to-char (first lst) -13) acc))]
		[(<= 97 (char->integer (first lst)) 109) (best-rot (rest lst) (cons (add-to-char (first lst) 13) acc))]
		[(<= 110 (char->integer (first lst)) 122) (best-rot (rest lst) (cons (add-to-char (first lst) -13) acc))]
		[else (best-rot (rest lst) (cons (first lst) acc))]))])
    (list->string (reverse (best-rot (string->list str) empty)))))

;; map defined in terms of foldr
(define (foldr-map fn lst)
    (foldr (lambda (x y) (cons (fn x) y)) empty lst))

(define (foldr-copy lst)
  (foldr cons empty lst))

(define (compose fn1 fn2)
  (lambda (x) (fn1 (fn2 x))))

(define (foldr-append lst1 lst2)
  (foldr cons lst2 lst1))

(define (foldr-length lst)
  (foldr (lambda (x y) (+ y 1)) 0 lst))

(define (foldr-sum lst)
  (foldr + 0 lst))

;; broken..needs to know the number of digits of the number n
(define (nth-digit n i)
  (let ([f (/ n (expt 10 (+ i 1)))])
    (floor (* 10 (- f (floor f))))))

(define (random-list n)
  (define (randlst i acc)
    (cond [(<= i 0) acc]
          [else (randlst (sub1 i) (cons (random n) acc))]))
  (randlst n '()))

(define (append-all a b)
  (cond [(and (list? a) (list? b)) (append a b)]
	[(list? a) (append a (list b))]
	[(list? b) (cons a b)]
	[else (list a b)]))

(define (my-append xs ys)
  (cond [(empty? xs) ys]
	[else (cons (first xs) (my-append (rest xs) ys))]))

(define (my-append2 xs ys)
  (define (my-append2-h sx acc)
    (cond [(empty? sx) acc]
	  [else (my-append2-h (rest sx) (cons (first sx) acc))]))
  (my-append2-h (reverse xs) ys))

(define (my-append3 xs ys)
  (foldr cons ys xs))

;; TODO: do the big-oh analysis of the flatten functions below
(define (my-flatten xs)
  (cond [(empty? xs) '()]
	[(list? (first xs)) (append (my-flatten (first xs)) (my-flatten (rest xs)))]
	[else (cons (first xs) (my-flatten (rest xs)))]))

(define (my-flatten2 xs)
  (define (my-flatten2-h xs acc)
    (cond [(empty? xs) acc]
	  [(list? (first xs))
	     (my-flatten2-h (rest xs) (append (my-flatten2-h (first xs) '()) acc))]
	  [else (my-flatten2-h (rest xs) (cons (first xs) acc))]))
  (reverse (my-flatten2-h xs '())))

(define (rpn-calc xs)
  (define (symbol->operator s)
    (cond [(eq? s '+) +]
          [(eq? s '-) -]
          [(eq? s '*) *]
          [(eq? s '/) /]))
  (define (symbol-urinary-op? s)
    (cond [(eq? s '!) #t]
          [else #f]))
  (define (symbol-urinary->op s)
    (cond [(eq? s '!) (lambda (n) (foldr * 1 (if (>= n 0) (range 1 (+ n 1)) (range n 0))))]))
  (define/match (rpn ys acc)
    [('() (cons a '())) a]
    [('() _) (error "Not a valid RPN expression")]
    [((cons y ys) acc) #:when (number? y) (rpn ys (cons y acc))]
    [((cons y ys) (cons a acc)) #:when (symbol-urinary-op? y)
     (rpn ys (cons ((symbol-urinary->op y) a) acc))]
    [((cons y ys) (cons a1 (cons a2 acc))) #:when (symbol? y)
     (rpn ys (cons ((symbol->operator y) a2 a1) acc))])
  (rpn xs '()))
