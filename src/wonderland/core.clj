(ns wonderland.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(println (foo "Marta!"))

(defn add [p1 p2] (+ p1 p2))

(println (add 1 2))

(println ((fn [] (str "hola lola" ", como estas?"))))

(def add (fn [p1 p2] (+ p1 p2)))
(println (add 3 4))

(println (#(str "hola lola" ", como estas?" %) "!"))

(println "martaRange *********************")

(defn martaRange [inici final]
  (cons inici (if (< (inc inici) final)  
                (martaRange (inc inici) final)
                nil)))

(println (martaRange 1 5))

(println (= (martaRange 1 4) '(1 2 3)))
(println (= (martaRange -2 2) '(-2 -1 0 1)))
(println (= (martaRange 5 8) '(5 6 7)))

(println "range2 *********************")

(def range2 #(loop [len (- %2 %1) r (list %1)]
	(println %1 %2)
	(println len r)
   	(if (= len 1) (reverse r)
     	(recur (dec len) (conj r (+ 1 (first r)))))))

(println (range2 1 5))

(println "range3 *********************")

(defn range3 [from to]
  (take-while #(< % to)
              (iterate inc from)))

(println ((fn [from to]
  (take-while #(< % to)
              (iterate inc from))) 1 5) )

(println (range3 1 5))

; lazy seqs
(println "lazy seqs ********************")

(println (take 5 (iterate inc 5)))

(println "range4 *********************")

(defn range4 [x y] (take (- y x) (iterate inc x)))

(println (range4 1 5))

(println "accessing maps **************")

(def k :a)
(def m {:a nil :b 2})

(println k)
(println m)
(println (m k))
(println (m :b))
(println "contains?" (contains? m :b))

(defn isValueNil? [k, m]
  (if (contains? m k)
	(nil? (m k))
   	false))

(println "isValueNil?" (isValueNil? :a m))

(println (m :b true))

(defn constMap [d, m]
  (reduce #(merge %1 {%2 d}) {} m))

(println (constMap 0 [:a :b :c]))

(defn constMap2 [d, m]
  (reduce #(assoc %1 %2 d) {} m))

(println (constMap2 0 [:a :b :c]))

(defn constMap3 [d, m]
	(apply assoc {}
		(interleave m (repeat d))))

(println (constMap3 0 [:a :b :c]))

(println ((comp first reverse) [1 2 3 4 5]))
(println (#(-> % reverse first) [1 2 3 4 5]))
(defn getLast[x]
  (if-let [r (next x)]
    (recur r)
    (first x)))
(println (getLast [1 2 3 4 5]))

(defn len [col]
  (loop [coll col
         c 1]
    (if (not (next coll))
      c
      (recur (next coll) (inc c) ) ) ) )

#(let [c 0] reduce (+ c 1) %)

(defn inv [col]
  (loop [pend (rest col)
         res (cons (first col) nil)]
    (if (= 0 (count pend))
      res
      (recur (rest pend) (cons (first pend) res)))))

(defn inv2 [col]
	(reduce #(cons %2 %1) '() col))

(println (inv '(1 2 3)))
(println (inv2 '(1 2 3)))
