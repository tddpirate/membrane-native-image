(ns pod.tddpirate.condwalk-test
  (:require
   ;;[pod.tddpirate.membrane :as sut] ;; sut = Software Under Test
   [pod.tddpirate.condwalk :as sut] ;; sut = Software Under Test
   [clojure.test :refer [deftest is testing]]))

(deftest test-basic-tests
  (testing "context 1 -FAIL"
    (is (= 0 1))
    )
  )


(def non-simple-obj?
  (complement sut/simple-obj?))


(deftest test-simple-obj
  (testing "simple-obj - true"
    (is (sut/simple-obj? 42))                               ;; number
    (is (sut/simple-obj? "Forty-two"))                      ;; string
    (is (sut/simple-obj? \F))                               ;; char
    (is (sut/simple-obj? :seventy-four))                    ;; keyword
    (is (sut/simple-obj? ::seventy-five))                   ;; keyword
    (is (sut/simple-obj? 'seventy-three))                   ;; symbol
    (is (sut/simple-obj? nil))                              ;; nil
    (is (sut/simple-obj? '(3 ::proxy "simply")))            ;; list
    (is (sut/simple-obj? (list 'a 'b nil 3 4)))             ;; list
    (is (sut/simple-obj? [4 7 \v]))                         ;; vector
    (is (sut/simple-obj? #{1 4 9 "sixteen" :twenty-five}))  ;; set
    (is (sut/simple-obj? { :k1 "val1" \k "val2" 3 9 }))     ;; map
    (is (sut/simple-obj? (clojure.lang.MapEntry. 1 2)))     ;; clojure.lang.MapEntry
    (is (sut/simple-obj? (range 5)))                          ;; seq
    )
  (testing "simple-obj - false"
    (is (non-simple-obj? =))                                ;; function
    (is (non-simple-obj? list))
    ;;(is (non-simple-obj?   ;; !!! add more types, such as Java interop.
    ))


;; !!! Use with-redefs to redefine java.util.UUID/randomUUID
;; !!! Example: clj-kondo/test/clj_kondo/main_test.clj:2066:(with-redefs,,,)

