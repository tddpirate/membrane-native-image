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
    (is (= simple-obj proxywannabe))
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


  
(deftest test-proxify-primitives
  (testing "proxify \"simple\" object"
    (run-test-proxify-simple 42)
    (run-test-proxify-simple "Forth")
    (run-test-proxify-simple :1984)
    (run-test-proxify-simple [5 8 \w]))
  (testing "proxify \"complicated\" object"
    (run-test-proxify-complicated #(str 'boo))
    (run-test-proxify-complicated assoc)))

(defn run-test-deproxify-approval-simple
  "Run proxy?*+proxy->obj tests on the proxy of a \"simple\" object"
  [simple-proxy-value]
  (let [tmprevproxies (ref {})
        deproxywannabe (sut/proxy?* simple-proxy-value)]
    (is (or (nil? deproxywannabe) (false? deproxywannabe)) (str "approving " simple-proxy-value))
    (is (= simple-proxy-value (sut/proxy->obj tmprevproxies simple-proxy-value)) (str "transforming " simple-proxy-value))))

(defn run-test-deproxify-approval-complicated
  "Run a proxy?* test on the proxy of a \"complicated\" object"
  [obj]
  (let [tmprevproxies (ref {(str 'dummy-key2) obj})
        proxy-value {:pod.tddpirate.condwalk/proxy (str 'dummy-key2)}]
    (is (= obj (sut/proxy->obj tmprevproxies proxy-value)))))

(deftest test-deproxify-approval
  (testing "deproxify-approval \"simple\" object"
    (run-test-deproxify-approval-simple 43)
    (run-test-deproxify-approval-simple "Fifth")
    (run-test-deproxify-approval-simple 1985))
  (testing "deproxify-approval \"complicated\" object"
    (run-test-deproxify-approval-complicated ref)
    (run-test-deproxify-approval-complicated str)))

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


(deftest test-proxify
  (is (= 1 (sut/proxify 1)))
  (is (= '("a" :b 7) (sut/proxify '("a" :b 7))))
  (let [proxy1 (sut/proxify '(+ inc))
        keyplus (get @sut/proxies +)
        keyinc (get @sut/proxies inc)]
    (println "!!! DEBUG test-proxify: proxies =" @sut/proxies "revproxies =" @sut/revproxies)
    (is (= (list {:pod.tddpirate.condwalk/proxy keyplus} {:pod.tddpirate.condwalk/proxy inc}) proxy1))))

(deftest test-deproxify
  (is (= 1 (sut/deproxify 1)))
  (is (= '("c" :d 8) (sut/deproxify '("c" :d 8))))
  (let [keyminus (:pod.tddpirate.condwalk/proxy (sut/proxify -))
        keydec (:pod.tddpirate.condwalk/proxy (sut/proxify dec))]
    (println "!!! DEBUG test-deproxify: proxies =" @sut/proxies "revproxies =" @sut/revproxies)
    (is (= '(- dec) (sut/deproxify (list {:pod.tddpirate.condwalk/proxy keyminus} {:pod.tddpirate.condwalk/proxy keydec}))))))


