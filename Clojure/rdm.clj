;; File: rdm.clj
;; Author: Collin J. Doering <rekahsoft@gmail.com>
;; Description: A rdm file for playing around with

(map (fn [x] (* x x)) '(0 1 2 3 4 5))

;; Example factorial function with accumulator (tail call)
(defn factorial
  ([n]
     (factorial n 1))
  ([n acc]
     (if  (= n 0) acc
          (recur (dec n) (* acc n)))))
