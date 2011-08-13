;; Exercise 1.2
(/ (+ 5 4 ( - 3 (- 3 (+ 6 1/5))))  (* 3 (- 6 3) (- 2 7)))
;; Exercise 1.3
(defn square [n] (* n n))
(defn sum-of-squares-of-larger [a b c] 
(if (and (< a b) ( < a c)) 
   (+  (square b) (square c))
  (if (and (< b c) ( < b a))
   (+  (square a) (square c))
  (if (and (< c b) ( < c a))
   ( + (square b) (square a))
  (+ (square a) (square c))))))
 (defn mysum [ b n] (+ b n)) 
  ;; Exercise 1.4
  (defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))
  ;;What happens here is that when the if form is evaluated it either returns "+" or "-". Because it returned value is not
  ;; enclosed in  parens ... it is then not evaluated. However, now it has two arguments, which are evaluated and applied to it.
  ;; Exercise 1.5
  ;;For applicative-order .. this will lead to infinite loop and test will never be evaluated.
  ;;For normal-order ... test will be evaluated .. 
    
  (defn p []  (p))

(defn mytest [x y]
  (if (= x 0)
      0
      y))
 
 (defn abs [x] (if (< x 0) (- 0 x)  x))
 (defn average [n m] (/ (+ n m) 2))
 
 (defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))
  
 (defn good-enough? [guess oldguess]
  (< (abs (-  guess  oldguess)) 0.01))

(defn improve [guess x]
  (average guess (/ x guess)))
 
 (defn sqrt-iter [guess  x ]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) 
                 x)))
 
 
  (defn sqrt-iterm [guess oldguess x ]
  (if (good-enough? guess oldguess)
      guess
      (sqrt-iterm (improve guess x) guess
                 x)))
 
(defn sqrtv [x]
  (sqrt-iter 1.0  x))
  
 (defn sqrtm [x]
  (sqrt-iterm 1.0  1.1  x)) 

(defn new-if [predicate then-clause else-clause]
  (cond [predicate then-clause]
        [ true else-clause]))
        
(comment       
(defn  sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
                     )

;; Excercise 1.6
;; What happens when Alyssa attempts to use this to compute square roots? Explain.
;;sqrt-iter will terminate because of stackoverflow. This is because function sqrt-iter will be called before application of new-if. 
;;Therefore, this means that new-if is not even an alternative to if
;; Excercise 1.7
 ;;The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. 
 ;;Also, in real computers, arithmetic operations are almost always performed with limited precision. 
 ;;This makes our test inadequate for very large numbers. 
 ;;Explain these statements, with examples showing how the test fails for small and large numbers. 
 ;;An alternative strategy for implementing good-enough? 
 ;;is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?
;; For the first part of the question ... small value like 0.0009,  0.04030062264654547 returns a wrong value and big value like 100000000, 1000.0000000000118

 (defn good-enoughm? [guess oldguess]
  (< (abs (-  guess  oldguess)) 0.01))

  (defn sqrt-iterm [guess oldguess x ]
  (if (good-enoughm? guess oldguess)
      guess
      (sqrt-iterm (improve guess x) guess
                 x)))
                 
(defn sqrtm [x]
  (sqrt-iterm 1.0  1.1  x)) 

;; 

;;Exercise 1.8.  
  (defn cimprove [guess x]
  (/ ( + (/ x (square guess)) (* 2 guess))  3))

  (defn crt-iterm [guess oldguess x ]
  (if (good-enoughm? guess oldguess)
      guess
      (crt-iterm (cimprove guess x) guess
                 x)))
                 
   (defn crtm [x]
  (crt-iterm 1.0  1.1  x))               

  
  