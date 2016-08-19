(ns calc.core-explained
  (:require [calc.tokens :as t]
            [clojure.test :refer [deftest testing is run-tests]]))
;; I've used "println" in a lot of places for debugging
;; I think it's wrong way
;; But here we can see how exactly it works

;; <low===high>
(def precedence ["+" "-" "*" "/"]) 
;; 1 + (2 * (2 + 2)) - (3 * 3)
 
;; since we're not manually handle every operator, we could use any 
;; clojure's function as well! And since the syntax allows us use a lot of
;; characters as "whitespace", we really could do such: "5 + |3 max 2|"
;; (in case we tokenized "max")
;; and get 8! Wow?! Nice sideeffect! (also we could use our own funcs^.^)
(defn count! [arg1 func arg2]
  (println "===="arg1 ":"func ":"arg2 "====")
  (eval (read-string (str "(" func " " arg1 " " arg2 ")"))))


(defn calc 
  ([tks] (calc tks precedence)) 
  ([tks [p & p-rest]]
   (let [[bef [oper & aft]] (split-with #(not (#{p} %)) tks)] ;; bad
     (cond
      oper (do (println bef ":" oper ":" aft) 
                          (count! (calc bef)
                                  oper
                                  (calc aft)))
      p-rest (recur tks p-rest)
      :otherwise (do (println (first bef)) 
                     (first bef)))))) 

;; folds all the parens, we could implement it through just
;; "find me the last parens and fold them" func and iterate on the tokens
;; until all the parens are gone. I think it'd be easier to understand
;; How do you think ?
(defn calc-paren [tks]
  (let [[bef [paren & aft]] (split-with #(not (#{"(" ")"} %)) tks)] ;; bad
    (do (println tks)
        (case paren
          ")" (->> (calc bef)
                   (str)
                   (conj aft))
          "(" (->> [bef (calc-paren aft)] ;; bad
                   (flatten)
                   (recur))
          nil tks))))


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
