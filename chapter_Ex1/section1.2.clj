;;Exercise 1.9
(comment (defn + [ a b]
  (if (= a 0)
      b
      (inc (+ (dec a) b)))))
;;(+  4 5)
;;(inc (+ 3 5))
;;(inc (inc (+ 2 5)))
;;(inc (inc (inc (+ 1 5))))
;;(inc (inc (inc (inc 5))))
;;(inc (inc (inc 6)))
;;(inc (inc 7))
;;(inc 8)
;;9
;;recursive
(comment (defn + [ a b]
  (if (= a 0)
      b
      (+ (dec a) (inc b)))))
;;(+ 4 5)
;;(+ 3 6)
;;(+ 2 7)
;;(+ 1 8)
;; 9
;;iterative
;; Exercise 1.10
(defn A [x y]
  (cond 
        (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))
  ;;(A 1 10) 1024
  ;;(A 2 4) 65536
  ;;(A 3 3) 65536
  
  (defn f [n] (A 0 n))
  ;;This function is Y*Y
  (defn g [n] (A 1 n))
  ;;This function is 2^y
  (defn h [n] (A 2 n))
  ;;
  (defn k [n] (* 5 n n))
  ;;This function is 5n^2
  
;;Exercise 1.11
(defn myfunc [n]
(cond
  (< n 3) n
  :else (+ (myfunc (- n 1)) (* 2 (myfunc (- n 2))) (* 3 (myfunc (- n 3))))))
  
(defn myfunc-iter [a b c n counter]
(cond
(= (- n counter) 2)   a
( < n 3) n
 :else  (myfunc-iter(+ a (* 2 b)  (* c 3) ) a b n (+ counter 1))))
 
 (defn myfunci [n] (myfunc-iter 2 1 0 n 0))
 ;;Exercise 1.12
 (defn fact [n]
 (cond
   (= n 0) 1
   (= n 1) 1
   :else (* n (fact ( - n 1)))))
 (defn pascal [n counter]
 (cond 
   (= n 0) '(1)
   (= n counter) (cons 1 (pascal n (+ counter 1)))
    (> counter n) nil
    :else (cons (/ (fact n)  (* (fact counter) (fact (- n  counter)))) (pascal n (+ counter 1)))))

;;Exercise 1.13.  Prove that Fib(n) is the closest integer to n/5, where  = (1 + 5)/2. Hint: Let  = (1 - 5)/2.
;;Use induction and the definition of the Fibonacci numbers (see section 1.2.2) to prove that Fib(n) = (n - n)/5
;;This is to be done later


;;Exercise 1.14.  Draw the tree illustrating the process generated by the count-change procedure of section 1.2.2 
;;in making change for 11 cents. 
;;What are the orders of growth of the space and number of steps used by this process as the amount to be changed increases?

;;(count-change 11)
;;(+ (cc 11 4) (cc -39 5))
;;(cc -39 5) -> 0
;;(cc 11 4) -> (+ (cc 11 3) (cc -14 5))
;;(cc -14 5) -> 0
;;(+ (+ (cc 11 3) 0) 0)
;;(+ (cc 11 2) (cc 1 3))
;;(cc 1 3) -> (+ (cc 1 2) (cc 0 3))
;;(+ (cc 1 2) 1)
;;(+ (+ (cc 1 1) (cc 0 1)) 1)
;;(+ (+ (+ (cc 1 0) (cc 0 0) 1) 1)
;;To be done later


;;Exercise 1.16
(defn square [n]
(* n n))
(defn fast-expt [base a n]
(cond
  (= n 1) a
  (even? n) (fast-expt base (* a (square base)) (/ n 2))
  :else  (fast-expt base (* a base) (- n 1))))

;;Exercise 1.17
(defn dubles [a]
(* 2 a))
(defn halve [b]
(/ b 2))
(defn mymult [ a b]
(cond
   (= b 1) a 
    :else (mymult (dubles a) (halve b))))
;;Exercise    
(comment (defn fib-iter [a b p q count]
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b  
                   <??>      ; compute p'
                   <??>      ; compute q'
                   (/ count 2)))
        :else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
  

;;Exercise 1.19
 ;;(defn fib [n]
  ;;(fib-iter 1 0 0 1 n))

;;Exercise 1.21

(defn divides? [a b]
  (= (rem b a) 0))
  
(defn next-m [n]
(cond 
(= n 2) 3
:else (+ n 2)))

(defn find-divisor [n test-divisor]
  (cond 
  (> (square test-divisor) n) n
  (divides? test-divisor n) test-divisor
    :else (find-divisor n (next-m test-divisor))))
    

  
   (defn smallest-divisor [n]
  (find-divisor n 2)) 
  
 (defn mprime? [n]
  (= n (smallest-divisor n))) 
  
 (defn search-of-primes [n]
(if (mprime? n)
(do (println n)
 (println "You are the one  " ))
 (search-of-primes (+ n 1))))
 
 (defn expmod [base exp m]
  (cond 
        (= exp 0) 1
        (even? exp) 
         (rem (square (expmod base (/ exp 2) m))
                    m)
        :else
         (rem (* base (expmod base (- exp 1) m))
                    m)))
                    
 (defn expmodn [base exp m]
  (rem (fast-expt base 1 exp) m))
 