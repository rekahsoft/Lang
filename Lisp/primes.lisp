;;; -*- Package: USER; Mode: LISP; Syntax: Common-lisp -*-
;;(in-package "USER")

;; Author: Jason Eisner <jason@cs.jhu.edu>, December 1993
;;
;; Use this code at your own risk; please give appropriate credit.
;; See primes.pdf for explanation and discussion.
;;
;; Because Common Lisp has bignum support built in, it is a handy
;; language for experimenting with very large prime numbers.

;;; *****************
;;; User macros to reload this file.
;;; *****************

(defparameter *workfile* "~/.scratch/primes")

(defmacro L (&optional (filename *workfile*))
   `(load ,filename))

(defmacro CL (&optional (filename *workfile*))
    `(progn (compile-file ,filename) 
            (load ,filename)))


;;; ***************************
;;; Useful macros & operations.
;;; ***************************

;; Logical implication.

(defmacro implies (antecedent consequent)
  `(or (not ,antecedent) ,consequent))


;; Accesses the nth value (counting from 0)
;; of a form that returns multiple values.
;;
;; This macro is supposed to be built into Common
;; Lisp, but it was left out of this implementation.

;;(defmacro nth-value (n form)
;;  `(nth ,n (multiple-value-list ,form)))


;;; *****************************
;;; Basic mathematical operations
;;; *****************************

(defmacro divides (a b)     ; returns T if a | b,  NIL otherwise
   `(zerop (mod ,b ,a)))

(defun square (x) (* x x))

;; returns a list of bits constituting the binary expansion of n,
;; from most to least significant.  T indicates a 1 bit, NIL a 0
;; bit.   Example: (binary-expansion 11) ==> (T NIL T T).

(defun binary-expansion (n)      
  (loop with answer = nil

        if (evenp n) do (push nil answer)
        else do (push t answer) and do (decf n)
        do (setf n (/ n 2))

        until (zerop n)
        finally (return answer)))


;; Quickly finds b^e (mod m).

(defun exptmod (b e m)    
  (if (zerop e) 1
    (mod (if (evenp e) 
             (square (exptmod b (/ e 2) m))
             (* b (exptmod b (1- e) m)))
         m)))


;; A bit faster than exptmod.  It's recursive rather than iterative, and 
;; the caller must provide a precomputed binary expansion of the exponent.
;;
;; (This is advantageous because we may have to reuse the same exponent
;; many times.)

(defun exptmod-fast (b binary-e m)    

  (loop with prod = 1
        for bit in binary-e
        do (setf prod (square prod))
        if bit do (setf prod (* b prod))
        do (setf prod (mod prod m))
        finally (return prod)))


;; Integer part of the cube root---avoids floating-point goofups.  
;; (See remarks at pi-Meissel.)

(defun icuberoot (n)
  (floor (expt (+ 0.5 n) 1/3)))


;; Integration by the trapezoidal rule.

(defun integrate (fn lower upper dx)    

  ;; correct dx so that we have a whole # of intervals
  (let* ((n (round (/ (abs (- upper lower)) dx)))
         (dx (/ (- upper lower) n)))
    (* dx (+ (loop for i from 1 to (- n 1)
                   for x from (+ lower dx) by dx
                   sum (funcall fn x))
             (* 1/2 (funcall fn lower))
             (* 1/2 (funcall fn upper))))))


;;; *****************
;;; Primality Testing
;;; *****************

;; Almost (but not quite) the stupidest primality test conceivable.

(defun prime?-slow (n)          
   (loop for i from 2 to (isqrt n)
      never (divides i n)))


;; Returns a list of all primes in [2,n], in increasing order.
;; Uses the stupid method prime?-slow to test numbers individually.
;; Doesn't amortize the work at all.

(defun primes-tested-slow (n)   
   (loop for i from 2 to n
         when (prime?-slow i)
         collect i))

(defun pi-tested-slow (n)
  (length (primes-tested-slow n)))


;; Returns an array holding the primes in [2,n], in increasing order.
;; 
;; The array has a fill pointer which registers its "active
;; length."  The active elements of the array are taken to be 0 
;; up to but not including the fill pointer.  (The built-in length 
;; function respects fill pointers.)
;;
;; An unproved theorem from Part II Number Theory says that
;; pi(n) is about n/log(n).  We initially allow for 1.2 times this
;; many elements, but we use an adjustable array that will automatically
;; get bigger if necessary.
;;
;; Notes:
;; 1. Takes no square roots.    (a significant linear speedup)
;; 2. Stops as soon as it knows a candidate is composite
;;    (divides by primes in the order 2, 3, 5, ... sqrt(candidate)).

(defun primes-tested (n)
   (let ((primes (make-array (floor (* 1.2 (/ n (log n)))) 
                             :fill-pointer 0      ; no primes in array yet
                             :adjustable t))      ; can get more space if necessary
         (n-divisors 0)          ; number of primes being used as divisors
         (i-biggest-testable 0)) ; largest integer testable using only
                                 ;   the first n-divisors primes as divisors

     (loop for i from 2 to n

           ;; First be very careful about the case where we're not sure
           ;; we have enough divisors to test i for primality.
           ;;
           ;; We can certainly test integers up to p^2 by using only
           ;; primes up to p.  
           ;;
           ;; Indeed, we can do slightly better most of the time:
           ;; we can test integers up to q^2-1, where q is the least
           ;; prime exceeding p.   (if one has been found!)
           ;;
           ;; The array holds all primes in [2,i-1], which is
           ;; always enough to test i ... so if n-divisors = (length primes),
           ;; we can proceed with a clear conscience, even if 
           ;; i > i-biggest-testable.
 
           do (if (and (> i i-biggest-testable)
                       (< n-divisors (length primes)))
                  (progn (incf n-divisors)
                         (setf i-biggest-testable
                               (if (< n-divisors (length primes))
                                   (1- (square (svref primes n-divisors)))
                                   (square (svref primes (1- n-divisors)))))))

           ;; Now see if i is a prime, and if so, add it to the array.

           (if (loop for index from 0 to (1- n-divisors)
                     never (divides (svref primes index) i))   ;; stops ASAP
               (vector-push-extend i primes)))

     primes))          



;; Finds pi(n) by using primes-tested.
;;
;; We only use primes-tested to generate the primes up to sqrt(n).
;; (We must test larger numbers for primality, of course, but we don't need 
;; to store them!)

(defun pi-tested (n)
  (let* ((sqrt[n] (max 2 (isqrt n)))
         (divisors (primes-tested sqrt[n])))
    (+ (length divisors)
       (loop for i from (1+ sqrt[n]) to n
             count (loop for index from 0 to (1- (length divisors))
                         never (divides (svref divisors index) i))))))



;;; *******************************
;;; Probabilistic primality testing
;;; *******************************

(defvar max-error-prob)
(defvar spot-checks)
(setf max-error-prob 1E-10)
(setf spot-checks (ceiling (- (/ (log max-error-prob) (log 4)))))

(defun Fermat-pseudoprime? (n base)
  (= 1 (exptmod base (1- n) n)))


;; A fast version: the caller must supply the binary-expansion of n-1.

(defmacro Fermat-pseudoprime?-fast (n base binary-e)  
  `(= 1 (exptmod-fast ,base ,binary-e ,n)))


;; Test whether n is a strong pseudoprime to the given base.
;; n-1 = t*(2^s), for t odd.  
;; binary-t is the binary-expansion of t.

(defun strong-pseudoprime? (n base binary-t s)   ;; where n-1 = t*(2^s)
  (let ((base^t (exptmod-fast base binary-t n)))
    (or (= 1 base^t)
        (loop with prod = base^t
              for r from 0 to (1- s)
              thereis (= (1- n) prod)
              do (setf prod (mod (square prod) n))))))


;; Pick an integer from [1, n) that is coprime to n.

(defun random-base (n)          
  (loop with base 
        do (setf base (1+ (random (1- n))))
        until (= 1 (gcd base n))
        finally (return base)))


;; The Miller-Rabin test.  Relies on the fact that if n is prime,
;; it is an strong pseudoprime to every base in [1, n), whereas if
;; it's composite, it is a strong pseudoprime to at most 1/4 of these
;; bases.
;;
;; We do enough random spot checks to bring the probability of an 
;; error below max-error-prob.

(defun prime?-probably (n)    
  (loop with tee = (1- n)
        with s = 0
        with binary-t
        initially (loop while (evenp tee) 
                        do (setf tee (/ tee 2))
                        do (incf s))     ;; now n-1 = 2^s * t with t odd
        initially (setf binary-t (binary-expansion tee))
        for i from 1 to spot-checks
        always (strong-pseudoprime? n (random-base n) binary-t s)))


;; Perfect testing of large primes, if the Generalized Riemann Hypothesis is true.
;; We only have to test against bases up to log n.

(defun prime?-GRH (n)

  (loop with tee = (1- n)
        with s = 0
        with binary-t
        initially (loop while (evenp tee) 
                        do (setf tee (/ tee 2))
                        do (incf s))     ;; now n-1 = 2^s * t with t odd
        initially (setf binary-t (binary-expansion tee))
        for base from 1 to (floor (log n))
        when (= 1 (gcd base n))
        always (strong-pseudoprime? n base binary-t s)))
        

(defun pi-tested-probably (n)
  (loop for i from 2 to n
        count (prime?-probably i)))


(defun pi-tested-GRH (n)
  (loop for i from 2 to n
        count (prime?-probably i)))


;; First prime after n.  To speed this up, we can look
;; in an arithmetic progression starting at n -- for example,
;; at the odd numbers after n.

(defun next-prime (n &optional (step 1))
  (assert (= (gcd n step) 1))
  (loop for i from n by step
        when (prime?-probably i)
        do (return i)))


;;; ****************
;;; Sieving Methods.
;;; ****************

;; Sieve the numbers from 1 to n, and return the resulting primes in a list.
;;
;; As a second value, we return an array with elements from 1 to n; the
;; primes are the elements with non-nil entries.

(defun primes-sieved (n)
   (loop with sieve = (make-array (1+ n) :initial-element T) 
                                          ; all elements initially flagged prime
         with divisor = 2
         while (<= (square divisor) n)  ; for all prime divisors < sqrt(n)

         ;; Strike out multiples of the divisor.
         do (loop for i from (+ divisor divisor) to n by divisor
                  do (setf (svref sieve i) nil))

         ;; Find the next non-prime divisor, if any.
         do (loop do (incf divisor)
                  until (or (svref sieve divisor) (> divisor n)))

         ;; At the end, collect up the remaining primes and return them.
         ;; Return the sieve itself as a second value.

         finally (setf (svref sieve 0) nil        ; these aren't primes either
                       (svref sieve 1) nil)     
                 (return (values (loop for i from 2 to n
                                       when (svref sieve i)
                                       collect i)
                                 sieve))))

(defun pi-sieved (n)
  (length (primes-sieved n)))


(defmacro link (i j)
  `(setf (svref next ,i) ,j
         (svref prev ,j) ,i))


;; A LINEAR algorithm.
;;
;; We keep a doubly linked list of the integers that might be
;; prime.  As multiples are struck out, they're removed from
;; this list.
;;
;; The list is implemented as a pair of arrays, prev and next.
;; If i is an integer on the list, and 1 <= k <= sqrt(n), then 
;;
;;    Prev[i]  = the next smallest list element (or 1 if none such)
;;    Next[i]  = the next largest list element (or n+1 if none such)
;;    S[k]     = the largest list element <= n/k  
;;    Sinv[i]  = {k: S(k) = i}    (stored as a list)
;;
;; Note: 2 is always the smallest list element; S[1] is always the largest 
;; list element.  Either fact would enable us to enumerate the primes in
;; the sieve (we don't currently).

(defun pi-sieved-linear (n)
   (loop with list-size = (1- n)   ;; all nos. in [2,n] are initially thought prime
                                  
         with sqrt-n = (isqrt n)
         with prev = (make-array (+ n 2)) 
         with next = (make-array (+ n 2))
         with Sinv = (make-array (+ n 1))
         with S    = (make-array (+ sqrt-n 1))

         initially (loop for i from 1 to n          ; set up linked list
                         do (link i (1+ i)))
                   (loop for k from 1 to sqrt-n     ; set up S and Sinv
                         for S[k] = (floor (/ n k))
                         do (setf (svref S k) S[k])
                            (push k (svref Sinv S[k])))

         with divisor = 2
         while (<= divisor sqrt-n)        ; for all prime divisors <= sqrt(n)

         ;; Multiplier starts at S[divisor] -- the biggest potential prime that, 
         ;; when multiplied by divisor, still yields a number <= n.   

         do (loop with multiplier = (svref S divisor)   
                  while (>= multiplier divisor)   ; needn't go smaller
                  for composite = (* multiplier divisor)
                  for predecessor = (svref prev composite)
                  for successor   = (svref next composite)

                  ;; now strike out the composite number.

                  do (link predecessor successor)
                     (decf list-size)
                     (loop for k in (svref Sinv composite)
                           do (setf (svref S k) predecessor)
                              (push k (svref Sinv predecessor)))

                  ;; move on to the next multiplier.

                  do (setf multiplier (svref prev multiplier)))
         
         do (setf divisor (svref next divisor))

         finally (return list-size)))


;; A variant of pi-sieved-linear, where we start with 
;; all multiples of 2 and 3 already struck out.
;; 
;; This optimization doesn't change the complexity of
;; the algorithm -- it just saves some cycles, by
;; eliminating two passes and by shortening the 
;; initialization of the arrays.  

(defun pi-sieved-linear-fast (n)
   (loop with list-size = (+ 1
                             (- n (floor (/ n 2)) (floor (/ n 3)))
                             (floor (/ n 6)))

         with sqrt-n = (isqrt n)
         with prev = (make-array (+ n 7)) 
         with next = (make-array (+ n 7))
         with Sinv = (make-array (+ n 1))
         with S    = (make-array (+ sqrt-n 1))

         initially (link 2 3)
                   (link 3 5)
                   (link 5 7)
                   (loop with i = 7
                         while (<= i n)
                         for j = (+ i 6)

                         do (link i (+ i 4))
                            (link (+ i 4) j)

                         do (setf i j))
                   (loop for k from 1 to sqrt-n     ; set up S and Sinv
                         for S[k] = (floor (/ n k)) ; might not be in the list
                         do (loop until (svref prev S[k])  
                                  do (decf S[k]))
                            (setf (svref S k) S[k])
                            (push k (svref Sinv S[k])))

         with divisor = 5

         ;; ** From here on, the code is exactly the same as in pi-sieved-linear. **

         while (<= divisor sqrt-n)        ; for all prime divisors <= sqrt(n)

         ;; Multiplier starts at S[divisor] -- the biggest potential prime that, 
         ;; when multiplied by divisor, still yields a number <= n.   

         do (loop with multiplier = (svref S divisor)   
                  while (>= multiplier divisor)   ; needn't go smaller
                  for composite = (* multiplier divisor)
                  for predecessor = (svref prev composite)
                  for successor   = (svref next composite)

                  ;; now strike out the composite number.

                  do (link predecessor successor)
                     (decf list-size)
                     (loop for k in (svref Sinv composite)
                           do (setf (svref S k) predecessor)
                              (push k (svref Sinv predecessor)))

                  ;; move on to the next multiplier.

                  do (setf multiplier (svref prev multiplier)))
         
         do (setf divisor (svref next divisor))

         finally (return list-size)))


;;; ******************
;;; Legendre's formula
;;; ******************

;; Computes phi(n, a), i.e., the number of integers in [1,n]
;;    not divisible by any of the first a primes.
;; Primelist is a list of the first a primes, in REVERSE order.

(defun phi (n a primelist) 
  (cond ((zerop a) (floor n))
        ((< n 1)   0)
        (t         (- (phi n (1- a) (rest primelist))
                      (phi (/ n (first primelist)) (1- a) (rest primelist))))))


;; Legendre's formula.

(defun pi-Legendre (n)
  (let* ((sqrt[n] (isqrt n))
         (pr      (primes-sieved sqrt[n]))
         (a       (length pr)))
    (+ a -1 (phi n a (reverse pr)))))



;; Returns an array holding the values of phi(t, k) for ALL 0 <= t <= m,
;; where m is the product of the first k primes.
;;
;; The first k primes must be provided.  We use a sieving method to 
;; determine the values efficiently, more or less as suggested in Riesel.

(defun phi-array (m primes)
  (let ((sieve (make-array (1+ m) :initial-element t)))

    ;; Cross out the integers that are divisible by at least one
    ;; of the given primes.

    (loop for p in primes
          do (loop for i from 0 to m by p
                   do (setf (svref sieve i) nil)))
    
    ;; Now change the entries of the sieve from logical flags to
    ;; numbers: at each entry, we count the number of previous entries
    ;; that are indivisible by all the primes, i.e., still have t entries.

    (loop for i from 0 to m
          count (svref sieve i) into phi
          do (setf (svref sieve i) phi))

    ;; Return the result.

    sieve))
          

(defvar *k* nil)          ;; Value of k from our last call to pi-Legendre-fast
(defvar *phi-array* nil)  ;; The phi-array we built then -- we may be able to
                          ;;   reuse it!    
(defvar *m* nil)          ;; The value of m we used then


;; Set the above global variables to be appropriate to a new
;; value of k.  If the value of k hasn't changed, leave the
;; old values alone (they're still appropriate.)

(defun initialize-phi-array (k)
    (unless (eq k *k*)      
            (let ((primes (loop ;; the first k primes (k is very small)
                                for i from 2
                                when (prime?-slow i)
                                collect i
                                and count T into count
                                until (= count k))))
              (setf *k*            k
                    *m*            (apply #'* primes)
                    *phi-array*    (phi-array *m* primes))))
    (values))


;; Given a phi-array of the correct form, computes phi quickly.
;; (See documentation at phi-array.)
;;
;; The optimization as described in the project handout uses
;; the Euler phi function of m.  Since we know m's factors, this
;; would be easy to find.  However, since it's exactly phi(m,k),
;; it was just as convenient to compute it as part of the phi-array.

(defun phi-fast (n a primelist &optional (k *k*) (m *m*) (phi-array *phi-array*))

  (cond ((= a k)
         (multiple-value-bind (s tee) (floor n m)   ;; so n = s*m + t,  0 <= t < m
            (+ (* s (svref phi-array m))
               (svref phi-array tee))))

        ((zerop a)       ;; another termination condition, in case we're 
         (floor n))      ;;  initially called with (a < k)

        ((< n 1) 0)

        (t
         (- (phi-fast n (1- a) (rest primelist)
                      k m phi-array)
            (phi-fast (floor n (first primelist)) (1- a) (rest primelist))))))


(defun pi-Legendre-fast (n &optional (k 6))   ;; k=0 gives vanilla pi-Legendre
  (let* ((sqrt[n]       (isqrt n))
         (primes        (primes-sieved sqrt[n]))
         (a             (length primes)))

    ;; Recompute the global variables unless they're appropriate from the last call.

    (initialize-phi-array k)
    (+ a -1 (phi-fast  n   a  (reverse primes)))))
             

;;; *****************
;;; Meissel's formula
;;; *****************

;; By scanning through a sieve, in linear time, creates an array 
;; of length n with a very special form: 
;;     If n is prime,     the nth element is pi(n).
;;     If n is not prime, the nth element is a negative value giving
;;                          the previous prime -- or 0 if none.

(defun pi-array (n)
  (loop with array = (nth-value 1 (primes-sieved n))
        with current-pi = 0
        with last-prime = 0
        for i from 0 to n
        if (svref array i)
        do (incf current-pi)
           (setf (svref array i) current-pi
                 last-prime      i         )
        else
        do (setf (svref array i) (- last-prime))
        finally (return array)))
        

;; Finds pi(n) by looking in a pi-array.

(defun lookup-pi (n pi-array)
  (let ((entry (svref pi-array n)))
    (if (> entry 0) 
        entry
      (svref pi-array (- entry)))))


;; Finds the greatest prime <= n, by looking
;; in a pi-array.  Returns 0 if none such.

(defun lookup-prime-floor (n pi-array)
  (let ((entry (svref pi-array n)))
    (if (> entry 0) n (- entry))))


;; Computes pi from Meissel's formula.
;;
;; Here pi-array has the form produced by the pi-array routine above.
;; It must have length AT LEAST sqrt(x).
;;
;; Important: We use an icuberoot function I've defined
;; to get the integer part of the cube root.  (This is analagous
;; to isqrt.)  If we don't do this, our answer is occasionally
;; off by one.  For example, pi(343) would be miscomputed, because
;; this computer thinks the cube root of 343 is 6.999999999999999.
;; So would any pi(n) that is computed in terms of pi(343).

(defun pi-Meissel (x &optional (pi-array (pi-array (isqrt x))))
  (if (<= x 1)
      0
    (let* ((cuberoot (icuberoot x))
           (sqrt     (isqrt x))
           (c        (lookup-pi cuberoot pi-array))
           (b        (lookup-pi sqrt     pi-array)))
      (initialize-phi-array 6)               ;; this call usually does nothing
      (+ (phi-fast x c 
                   (loop ; find the first c primes, in reverse order
                         with p = (lookup-prime-floor cuberoot pi-array)
                         until (zerop p)
                         collect p
                         do (setf p (lookup-prime-floor (1- p) pi-array))))
         (* 1/2 (+ b c -2) (- b c -1))
         (- (loop with p = (lookup-prime-floor sqrt pi-array)
                  while (> p cuberoot)
                  sum (pi-Meissel (floor x p) pi-array)
                  do (setf p (lookup-prime-floor (1- p) pi-array))))))))


;;; *******************
;;; Approximating pi(x)
;;; *******************

;; Computes Li(x) from its formal definition.

(defun Li-integrand-slow (s) (/ (log s)))
(defun Li-slow (x &optional (ds 1.0))
  (+ 1.045 (integrate #'Li-integrand-slow 2 x ds)))


;; Faster computation using a change of variable -- in the integration
;; above, we're using narrow intervals on an integrand that gets flatter
;; and flatter.

(defun Li-integrand (u) (/ (exp u) u))

(defun Li (x &optional (du 0.001))
  (+ 1.045 (integrate #'Li-integrand (log 2) (log x) du)))


;; The approximation to pi(x) given by the prime number theorem.

(defun log-approx (n)
  (/ n (log n)))


;;; ***********************
;;; Tabulating our results.
;;; ***********************

;; Calls the given function on n = 10, 20, 30, ... 90, 100, 200, ... 
;; up to upperbound (if supplied), throwing away any results.
;;
;; We use this to print a table.

(defun drive-tabulation (function &optional upperbound (factor 10))
  (loop with interval = factor
        with arg = interval
        while (implies upperbound (<= arg upperbound))
        for total-calls from 0
        do (funcall function arg)
           (if (and (divides (1- factor) total-calls) 
                    (> total-calls 0))
               (setf interval (* interval factor)))
           (incf arg interval))
  (values))


;; Calls all the functions on the argument, and collect the results
;; into a list.

(defun eval-fns (fns arg)
  (loop for fn in fns
        collect (funcall fn arg)))


;; Prints the print names of the list elements as column headers, splitting
;; them as readably as possible across several lines if necessary.

(defun print-headers (headers)
  (loop with done? = nil
        with names = (loop for object in headers
                           collect (string-capitalize (format nil "~A" object)))
        for print = (loop initially (setf done? t)
                          for i from 0 to (1- (length names))
                          for name = (elt names i)
                          for length = (length name)
                          for cut-point = (if (<= length 10)
                                              length
                                            (1+ (or (position #\- name :end 10 :from-end t)
                                                    9)))
                          for print-part = (subseq name 0 cut-point)
                          for remaining = (subseq name cut-point)  ;; to end

                          collect print-part
                          do (setf (elt names i) remaining)
                             (when (string/= remaining "") 
                                   (setf done? nil)))

        do (print-table-line "   " print)
        until done?)
  (values))


;; Prints the list as a table line.

(defun print-table-line (row-label list)
  (format t  "~&~A~{~,12T~A~}" row-label list))


;; Tabulates the values of a function for 10, 20, 30, ... 90, 100, 200, ...
;; Here fns may be either a single function or a list of functions.
;; Their print names will be used as the headers, unless a list of other
;; headers (strings) is provided as a keyword argument.
;;
;; There are some other optional keyword arguments.  The multiplicative factor 
;; may be specified, and so may an upper bound that says how far to tabulate.  
;; Finally, if the list upperbounds has an nth element, it serves as a
;; further upperbound on the nth function.

(defun tabulate (fns &key upperbounds 
                          (upperbound (when upperbounds 
                                            (apply #'max upperbounds))) 
                          (factor 10) 
                          headers)

  ;; If there's only a single function, make it a list of one function.
  ;; (Careful, because some function specifiers look like lists.)

  (unless (and (listp fns) 
               (not (eq (car fns) 'function))
               (not (eq (car fns) 'lambda)))
          (setf fns (list fns)))
 
  (print-headers (or headers fns))

  (drive-tabulation
      #'(lambda (arg)
          (print-table-line arg
            (loop for fn in fns
                  for i from 0
                  for upperbound = (nth i upperbounds)
                  when (implies upperbound (<= arg upperbound))
                  collect (funcall fn arg)
                  else collect "   ")))
      upperbound factor))


;; Tabulates all our methods as far as appropriate.

(defun tabulate-pi-methods ()
  (tabulate  '(pi-tested-slow   pi-tested             pi-tested-probably pi-tested-GRH    pi-sieved
               pi-sieved-linear pi-sieved-linear-fast pi-Legendre        pi-Legendre-fast pi-Meissel)
:upperbounds '(    100000           100000              10000               10000           500000
                   100000           100000            1000000            10000000         10000000)))


;; Tabulates approximations to pi.

(defun tabulate-approximations ()
  (print-headers '("        pi" "   x/log x" "        li" " log-ratio"
                   "  li-ratio" "  log-diff" "   li-diff"))
  (drive-tabulation
     #'(lambda (n)
         (format t  "~&~A~{~,12T~11,3F~}" n
           (let ((pi-true (pi-Meissel n))
                 (pi-log  (log-approx n))
                 (pi-li   (Li n)))
             (list pi-true pi-log pi-li (/ pi-true pi-log)
                   (/ pi-true pi-li) (- pi-true pi-log) (- pi-true pi-li)))))
     10000000))

