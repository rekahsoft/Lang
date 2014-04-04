;; File: rdm.lisp
;; Date: 28/08/2010
;; Author: Collin J. Doering <rekahsoft@gmail.com
;; Description: Random source file to experiment while learning common lisp

(defun factorial (x &optional (acc 1))
  (cond ((<= x 1) acc)
	(T (factorial (- x 1) (* acc x)))))

;; perhaps a nicer factorial function which instead of having
;; the accumulator as a optional variable accessable by the user
;; hides it internally using labels
(defun factorial1 (x)
  (labels ((factorial1-helper (n acc)
	     (if (<= n 0)
		 acc
		 (factorial1-helper (- n 1) (* acc n)))))
    (factorial1-helper x 1)))

;; Old version of pow depreciated becauseit failed to hide the accumulator
;; (defun pow (x n &optional (acc 1))
;;   (cond ((= n 0) acc)
;; 	((> n 0) (pow x (- n 1) (* acc x)))
;; 	((< n 0) (pow x (+ n 1) (* acc (/ 1 x))))))

(defun pow (x n)
  (labels ((pow-helper (x n acc)
	     (cond ((= n 0) acc)
		   ((> n 0) (pow-helper x (- n 1) (* acc x)))
		   ((< n 0) (pow-helper x (+ n 1) (* acc (/ 1 x)))))))
    (pow-helper x n 1)))

(defun bad-factorial (x)
  (cond ((<= x 0) 1)
	(T (* x (bad-factorial (- x 1))))))

(defun fib (n)
  (let ((fib-dot-lst nil))
    (labels ((gen-fib (n x)
	       (cond ((> x n) (car fib-dot-lst))
		     ((= x 0) (setf fib-dot-lst (cons 0 nil))
		              (gen-fib n (+ x 1)))
		     ((= x 1) (setf fib-dot-lst (cons 1 0))
			      (gen-fib n (+ x 1)))
		     (t (let* ((fst (car fib-dot-lst))
				  (scd (cdr fib-dot-lst))
				  (fibx (+ fst scd)))
				  (setf fib-dot-lst (cons fibx fst))
		                  (gen-fib n (+ x 1)))))))
      (gen-fib n 0))))

(defun my-cat (pathd)
  (with-open-file (pathd-in pathd)
     (loop for line = (read-line pathd-in nil)
	   while line do (format t "~a~%" line))))

(defun average-list (lst &optional (acc 0) (len 0))
  (cond ((null lst) (if (> len 0) (/ acc len)))
	(T (average-list (rest lst) (+ acc (first lst)) (1+ len)))))

(defun interval (a b)
  (labels ((interval-helper (a b &optional (acc nil))
	     (if (< b a) acc (interval-helper (+ a 1) b (cons a acc)))))
    (reverse (interval-helper a b))))

;; broken.. needs complete rewrite
;; (defun prime-seive (a b primes)
;;   (let ((a-to-b (interval a b)))
;;     (labels ((prime-seive-helper (inter primes &optional (acc nil))
;; 	       (if inter
;; 		   (loop for p in primes
;; 		         if (divides p (car inter))
;; 			 do (format t "~a -|- ~a~%" p (car inter))
;; 		      finally (prime-seive-helper (cdr inter) primes (cons (car inter) acc)))
;; 		   acc)))
;;       (prime-seive-helper a-to-b primes))))
