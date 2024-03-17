(ns com.phronemophobic.membrane.hello-world
  (:require [membrane.ui :as ui]
            [membrane.example.todo :as td]
            membrane.component 
            [membrane.java2d :as java2d])
  (:gen-class))

(defn -main [& _args]
  (println "Trying to print something")
  (println "java.library.path =" (java.lang.System/getProperty "java.library.path"))
  (java.lang.System/setProperty "java.library.path" (str "/home/omer/work/Projects/Clojure/BabashkaPodWork/membrane-native-image:" (java.lang.System/getProperty "java.library.path")))
  (println "java.library.path =" (java.lang.System/getProperty "java.library.path"))
  (java2d/run
    (membrane.component/make-app
     #'td/todo-app
     td/todo-state)))

