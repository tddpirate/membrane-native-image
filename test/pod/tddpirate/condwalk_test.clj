(ns pod.tddpirate.condwalk-test
  (:require
   ;;[pod.tddpirate.membrane :as sut] ;; sut = Software Under Test
   [pod.tddpirate.condwalk :as sut] ;; sut = Software Under Test
   [clojure.test :refer [deftest is testing]]))

#_(deftest test-basic-tests
  (testing "context 1 -FAIL"
    (is (= 0 1))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for
;; proxies for objects which cannot be properly
;; serialized and deserialized ("complicated" objects).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    )
  (testing "simple-obj - false"
    (is (non-simple-obj? (range 5)))                        ;; seq
    (is (non-simple-obj? =))                                ;; function
    (is (non-simple-obj? #(+ %1 %2)))                       ;; function
    ;;(is (non-simple-obj? list))
    ;;(is (non-simple-obj?   ;; !!! add more types, such as Java interop.
    ))

(defn run-test-proxify-simple
  "Run a obj->proxy test on a \"simple\" object"
  [simple-obj]
  (let [tmpproxies (ref {})
        tmprevproxies (ref {})
        tmpUUIDfunc #(str 'dummy-key)
        proxywannabe (sut/obj->proxy tmpproxies tmprevproxies tmpUUIDfunc simple-obj)
        ]
    (is (not proxywannabe))
    (is (= {} @tmpproxies))
    (is (= {} @tmprevproxies))))

(defn run-test-proxify-complicated
  "Run a obj->proxy test on a \"complicated\" object"
  [complicated-obj]
  (let [tmpproxies (ref {})
        tmprevproxies (ref {})
        tmpUUIDfunc #(str 'dummy-key)
        proxywannabe (sut/obj->proxy tmpproxies tmprevproxies tmpUUIDfunc complicated-obj)
        ]
    (is (map? proxywannabe))
    (let [proxyid (:pod.tddpirate.condwalk/proxy proxywannabe)]
      (is (= (str 'dummy-key) proxyid))
      (is (= complicated-obj (get @tmprevproxies proxyid)))
      (is (= proxyid (get @tmpproxies complicated-obj)))
      (is (= (list complicated-obj) (keys @tmpproxies)))
      (is (= (list (str 'dummy-key)) (keys @tmprevproxies))))))


  
(deftest test-proxify
  (testing "proxify \"simple\" object"
    (run-test-proxify-simple 42)
    (run-test-proxify-simple "Forth")
    (run-test-proxify-simple :1984)
    (run-test-proxify-simple [5 8 \w]))
  (testing "proxify \"complicated\" object"
    (run-test-proxify-complicated #(str 'boo))
    (run-test-proxify-complicated assoc)))

(defn run-test-deproxify-simple
  "Run proxy->obj test on the proxy of a \"simple\" object"
  [simple-proxy]
  (let [tmprevproxies (ref {})
        deproxywannabe (sut/proxy->obj tmprevproxies simple-proxy)]
    (is (or (nil? deproxywannabe) (false? deproxywannabe)))))

(defn run-test-deproxify-complicated
  "Run a proxy->obj test on the proxy of a \"complicated\" object"
  [obj]
  (let [tmprevproxies (ref {(str 'dummy-key2) obj})
        proxy {:pod.tddpirate.condwalk/proxy (str 'dummy-key2)}]
    (is (= obj (sut/proxy->obj tmprevproxies proxy)))))

(deftest test-deproxify
  (testing "deproxify \"simple\" object"
    (run-test-deproxify-simple 43)
    (run-test-deproxify-simple "Fifth")
    (run-test-deproxify-simple 1985))
  (testing "deproxify \"complicated\" object"
    (run-test-deproxify-complicated ref)
    (run-test-deproxify-complicated str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for
;; the condwalk function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn approve-always
  "Always approve the argument"
  [arg]
  true)

(defn number-inc
  "Increment the argument only if it is an argument.
  A string is enclosed inside \"earmuffs\"."
  [arg]
  (cond
    (number? arg) (inc arg)
    (string? arg) (str "*" arg "*")
    :else arg))


(deftest test-condwalk
  (testing "simple condwalk cases"
    (is (= 1 (sut/condwalk approve-always number-inc 0)))
    (is (= "*A*" (sut/condwalk approve-always number-inc "A")))
    (is (= [2 5 10 [17 26 [37]]] (sut/condwalk approve-always number-inc [1 4 9 [16 25 [36]]])))
    (is (= '(2 5 10 (17 26 (37))) (sut/condwalk approve-always number-inc '(1 4 9 (16 25 (36)))))))
  (testing "complicated condwalk cases"
    (is (= [2 5 10 [1 2] [4 5] 26] (sut/condwalk #((complement =) [1 2] %) number-inc [1 4 9 [1 2] [3 4] 25])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for
;; condwalk using our 'approve' and 'func' functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Our 'approve' function for proxify is simply 'simple-obj?'.
