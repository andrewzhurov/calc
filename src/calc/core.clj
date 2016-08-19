(ns calc.core
  (:require [calc.tokens :as t]
            [clojure.test :refer [deftest testing is run-tests]]))

(def precedence ["+" "-" "*" "/"]) 
(defn count! [arg1 func arg2]
  (eval (read-string (str "(" func " " arg1 " " arg2 ")"))))


(defn calc 
  ([tks] (calc tks precedence)) 
  ([tks [p & p-rest]]
   (let [[bef [oper & aft]] (split-with #(not (#{p} %)) tks)] ;; bad
     (cond
      oper   (count! (calc bef)
                     oper
                     (calc aft))
      p-rest (recur tks p-rest)
      :otherwise (first bef))))) 

(defn calc-paren [tks]
  (let [[bef [paren & aft]] (split-with #(not (#{"(" ")"} %)) tks)] ;; bad
    (case paren
      ")" (->> (calc bef)
               (str)
               (conj aft))
      "(" (->> [bef (calc-paren aft)] ;; bad
               (flatten)
               (recur))
      nil tks)))

(defn do-it [string]
  (->> string
       t/tokenizer
       calc-paren
       calc))




(deftest count!-test
  (testing "hard"
    (is (= (count! "4" "-" "-2") 6))
    (is (= (count! "-8" "/" "2") -4))))
(deftest calc-test
  (testing "hard"
    (is (= (calc ["2" "+" "2" "*" "2"]) 6))
    (is (= (calc ["2" "-" "-2" "*" "2"]) 6))))
(deftest calc-paren-test
  (testing "simple"
    (is (= (calc-paren ["(" "2" "+" "2" ")"]) '("4")))
    (is (= (calc-paren ["(" "(" "2" "+" "2" ")" "*" "2" ")"]) '("8")))
    (is (= (calc-paren ["(" "8" "/" "(" "2" "+" "2" ")" ")"]) '("2")))
    (is (= (calc-paren ["(" "2" "+" "2" ")" "*" "(" "1" "+" "1" ")"]) '("4" "*" "2")))))

(run-tests)
