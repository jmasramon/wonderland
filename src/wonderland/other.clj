(ns wonderland.other)

(defn group [col]
  (loop [res '()
        tmp  (cons (first col) nil) 
        pend (rest col)]
  (if (= 0 (count pend))
    (reverse (cons tmp res))
    (if (not (= (last tmp) (first pend)))
      (recur (cons tmp res) (cons (first pend) nil) (rest pend))
      (recur res (cons (first pend) tmp) (rest pend))))))

(defn group2 [col]
	(partition-by identity col))

(defn group3 [col]
	(loop [ res '()
        	pend  col]
        (if (empty? pend)
        	(reverse res)
        	(recur 
        		(cons (take-while #(= (first pend) %) pend) res) 
        		(drop-while #(= (first pend) %) pend)))))

(println (group [1 1 2 1 1 1 3 3]))
(println (group2 [1 1 2 1 1 1 3 3]))
(println (group3 [1 1 2 1 1 1 3 3]))

 (reverse (reduce #(if (odd? %2) (cons %2 %1) %1) '() '(1 2 3)))

 (filter odd?)

 ; palindrome detecotors ***********************************

; break in two, reverse and compare
 (defn palindrome? [col]
 	(let [half (quot (count col) 2)]
		(= (take half col) (reverse (take-last half col)))))

 (println (palindrome? '(1 2 3 2 1)))
 (println (palindrome? '(1 2 2 1)))
 (println (palindrome? "racecar"))
 (println (palindrome? '(1 2 1 1)))
 
 (defn palindrome2? [col]
 	(= (seq col) (reverse col)))

 (println (palindrome2? '(1 2 3 2 1)))
 (println (palindrome2? '(1 2 2 1)))
 (println (palindrome2? "racecar"))
 (println (palindrome2? '(1 2 1 1)))

; fibonnacci

(defn fibonnacci [n]
	(case n
		1 1
		2 1
		(+ (fibonnacci (- n 2)) (fibonnacci (- n 1)))))

(defn fibSeries [n]
	(loop [m n
		acc '()]
		(if (= 0 m)
			acc
			(recur (dec m) (cons (fibonnacci m) acc)))))

(println (fibonnacci 4))
(println (fibSeries 4))

; (defn fibSeries2 
; 	(take % 
; 		((fn fib [a b] 
; 			(lazy-seq (cons a (fib b (+ a b))))) 
; 			1 1))

(defn maximum [& args] (reduce #(if (> %2 %1) %2 %1) args))

(defn maximum2 [& args] ((comp last sort) args))

(def max3 (comp last sort list))

(println (maximum2 3 2 4 1 7))
(println (max3 3 2 4 1 7))

; getting just capitals

(defn justCap [col] (clojure.string/join "" (filter #(Character/isUpperCase %) (seq col))))
(def justCap2 #(apply str (re-seq #"[A-Z]" %)))

(println (justCap "HolaLola"))
(println (justCap2 "HolaLola"))

