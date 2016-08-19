(ns calc.tokens
  (:require [clojure.test :refer [is testing deftest run-tests]]))

(def whitespace #"[ \n\t]+")
(def tokens '(#"-?\d+"
              #"[\*\/\-\+]"
              #"\("
              #"\)"))
(def re (->> tokens
             (map #(str % "|"))
             (apply str)
             (#(str % whitespace)) ;; bad
             (re-pattern))) 

(defn tokenizer [expr]
  (->> expr
       (re-seq re)
       (remove clojure.string/blank?)))


(deftest tokenizer-test
  (testing "simple"
    (is (= '("1" "+" "2")
           (tokenizer "1 + 2")))
    (is (= '("4" "*" "3" "/" "1")
           (tokenizer "4 * 3 / 1"))))
  (testing "complex"
    (is (= '("(" "2" "+" "-2" ")" "*" "2")
           (tokenizer "(2 +-2)  *2")))))

(run-tests)
