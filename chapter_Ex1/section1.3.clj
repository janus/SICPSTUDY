(defn mycube [n]
    (* n n n))


(defn mysum [f a func b]
(if (> a b)
    0
  (+ (f a) (mysum f (func a) func b))))
  
  
(mysum mycube 1 inc 10)

(defn constanth [b a n]
   (/ (- b a) n))
(defn add-dx [ n ] (fn [r] (+ r (/ 1 n))))
(defn simpson-rule [ f a b n]
(* (/ (constanth b a n) 3) (mysum  f 0 inc  n)))

;;Ex 1.30
(defn isum [term a next b]
  (loop [ init a  result 0 end b ]
        (if (> init end)
           result
           (recur (next init) (+ result (term init)) end))))
;; iterative
;; Ex 1.31 a
(defn iproduct [term a next b]
  (loop [ init a  result 1 end b ]
        (if (> init end)
           result
           (recur (next init) (* result (term init)) end))))
           
(defn factorial [n]
(iproduct identity 1 inc n))

(defn inc-by-2 [n] (+ 2 n))

(defn mysquare [n] (* n n))

(/ (* 3 8 ( iproduct mysquare 4 inc-by-2 7)) ( iproduct mysquare 3 inc-by-2 7))
;; recursive
;; Ex 1.31 b

(defn reproduct [term a next b]

   (if (> a b)
      1
      
   (* (term a)  (reproduct  term (next a) next b))))

;; Ex 1.32 a
(defn raccumulator [combiner null-value term a next b]
   
      (if (> a b)
         null-value
         (combiner (term a) (raccumulator combiner null-value term (next a) next b))))
 
;; Ex 1.32 b 
(defn iaccumulator [combiner null-value term a next b]
     (loop [ result null-value init a end b]
     (if (> init end)
        result
        (recur (combiner result (term init))  (next init)  end))))
        
 ;; Ex 1.33
(defn filter-acc [pred combiner null-value  a next b]
  (if (> a b)
    null-value
    (if (pred a)
        (combiner a (filter-acc pred combiner null-value  (next a) next b))
        (filter-acc pred combiner null-value  (next a) next b))))

;;Ex 1.34

(defn f [g]
(g 2))

;; When we call (f f) .. the function will run and the body of the function will change to (f 2)... 
;; So it would recurse ...with 2 as the argument.. when it re-enters the body... It would be (2 2)
;;which means a primitive value in place of a function.. and it would fail then.
(defn abs [n]
(if (< n 0)
  (* n -1)
  n))

(defn fixed-point [f first-guess]
   (let [ tolerance 0.00001 close-enough? (fn [v1 v2] (< (abs (- v1 v2)) tolerance))]
       (loop [guess first-guess mynext (f first-guess) ]
            (print mynext "\n" )
            (if (close-enough? guess mynext)
                 mynext
                 (recur  mynext (f mynext))))))

(defn average [ y x]
   (/ (+ y x) 2))
   
(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y))) 1.0))
 
;;Ex 1.35 
  
(defn golden-ratio []
(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0))

;;Ex 1.36
(defn log [] )

(defn mylog-x []
(fixed-point (fn [x] (/ log(1000) log(x))) 2))
;; I don't understand the case of avrage here

(defn myflatten [result my-vec]
(if (seq my-vec)
  (do
   (if (= (str (class (first my-vec)))  "class clojure.lang.LazilyPersistentVector")
     (myflatten  (into result  (myflatten [] (first my-vec))) (rest my-vec))
     (myflatten (into result [(first my-vec)]) (rest my-vec))))
        result))
;; Ex 1.37
(defn con-frac-defn [n d k i result]
(loop [ i (inc i) result (/ (n i) (+ result (d i))) ]
  (if (= i k)
    result
   (recur (inc  i ) (/ (n i) (+ result (d i))))))) 

(defn cont-frac [n d k]
(con-frac-defn n d k 1 (/ (n 0) (d 0))))
;; Not working yet
(defn rcon-frac-defn [result n d k i ]
  (if (= i k)
    (/ ( n i) result)
   (+ ( / (n i) (+ result (d i))) (rcon-frac-defn  ( / (n i) (+ result (d i))) n d k (inc i) )))) 




(defn rcont-frac [n d k]
(rcon-frac-defn (/ (n 0) (d 0)) n d k 1 ))
(defn d-fun [x]
  (cond 
  (= 0 x)  1
  (= 1 x)  2
  (= (mod (- x 1)  3) 0) (* 2 (+  (/ (- x 1) 3) 1))
  :else 1))
  
  
  
  (def dx 0.00001)
  
  (defn deriv [g]
    (fn [x]
         (/ (- (g (+ x dx)) (g x)) dx)))
  
  (defn   newton-transform  [g]
   (fn [x] (- x (/ (g x) ((deriv g) x)))))
  
  (defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))
  
;; 1.38

(defn leohnard-euler [n d k ]
(con-frac-defn n d k 1 (/ (n 0) (d 0))))

;; Ex 1.39  Uncompleted
(comment
(defn tan-cf [ x k]
(loop [ i (- i 2) result (/ (n i) (+ result (d i))) ]
  (if (= i k)
    result
   (recur (inc  i ) (/ (n i) (+ result (d i))))))) 
   )
   
;; Ex 1.40   
(defn cubic [a b c]
(fn [x] (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 3 7 8) 1)

(defn square [x] (* x x ))

;; Ex 1.41
(defn my-double [f]
  (fn [x] (f (f x))))

;; Ex 1.42
(defn my-compose [f g]
(fn [x] (f (g x))))

;; Ex 1.43
(defn my-repeated [ f n]
(loop [ result (if (>= n 2) (my-compose f f) (if (= n 0) 0 (fn [x] (f x)))) n (if (> n 2) ( - n 2) 0)]
 (if (= n 0) result
 (recur (my-compose f result) (- n 1)))))
 
;;Ex 1.44
(defn smooth [f]
(fn [x] (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(defn n-fold-smooth [n x]
((my-repeated smooth n) x))

;;Ex 2.19
(defn no-more? [coll] (empty? coll))
(defn except-first-denomination [coll] (rest coll))
(defn first-denomination [coll] (first coll))
(defn cc [amount coll]
       (cond
          (= amount 0) 1
          (or (< amount 0) (no-more? coll)) 0
           :else  ( + (cc amount (except-first-denomination coll))
                       (cc (- amount (first-denomination coll)) coll))))
;; The arrangement of elements in the list does not affect the result ... Beacuse all elements are given equal chance

;;Ex 2.20

(defn bit? [b] (fn [n] (= 1 (bit-and b n))))

(defn same-parity [ & data] (let [fn-bit?  (bit? (first data )) coll (rest coll)]
                            (filter fn-bit? coll)))

;; Ex 2.21
(defn square [n] (* n n))

;; Using my baked recursive
(defn my-square-list [coll] (if (empty? coll) '() (cons (square (first coll)) (my-square-list (rest coll)))))

;; Usinf map

(defn square-list [coll] (map square coll))

;; Ex 2.22
;; The reason why the result list was a reverse because of the use of cons..
;; cons takes an item as the first argument and the second a list ... Here, the recursive pushes the result on top of 
;; the list
;; The second attempt failed woefully because of cons, whose second argument must be a list .. her it was not

(defn
