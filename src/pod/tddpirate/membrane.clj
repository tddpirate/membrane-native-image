(ns pod.tddpirate.membrane
  {:no-doc true}
  (:refer-clojure :exclude [read read-string])
  (:require [bencode.core :as bencode]
            [membrane.java2d]
            [membrane.ui]
            [membrane.component]
            [membrane.basic-components]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:require [pod.tddpirate.condwalk :refer [proxify deproxify]])
  (:import [java.io PushbackInputStream])
  (:gen-class))

(set! *warn-on-reflection* true)

(def debug? true)
(defn debug [& args]
  (when debug?
    (binding [*out* (io/writer "/tmp/debug.log" :append true)]
      (apply prn args))))

(def stdin (PushbackInputStream. System/in))

(defn write [v]
  (debug "--> writing bencode data:" v)
  (bencode/write-bencode System/out v)
  (.flush System/out))

(defn read-string [^"[B" v]
  (String. v))

(defn read []
  (bencode/read-bencode stdin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special variants of membrane functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-ns 'pod.tddpirate.x)
(intern (find-ns 'pod.tddpirate.x) 'run*
        (fn
          ;; "Used instead of java2d/run due to special handling of first argument"
          [constfn & arg]
          (debug "!!! DEBUG: invoking run*")
          (apply membrane.java2d/run (constantly constfn) arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a lookup table from a namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ns+symbol->2tuple
  "Transform a ns name and the name of a symbol inside it
  into a 2-tuple for insertion into a map."
  [podprefix ns symb] ;; podprefix is typically "pod.tddpirate."
  {:pre [(string? podprefix)
         (= (type ns) clojure.lang.Namespace)
         (symbol? symb)]}
  (let [fullpodns (str podprefix (str ns))]
    (debug "!!! DEBUG" "podprefix =" podprefix "ns =" ns "symb =" symb "fullpodns =" fullpodns "!!! DEBUG (end)")
    (debug "!!!------------")
    (debug "!!! symbol 1" (symbol fullpodns (str symb)))
    (debug "!!!------------")
    (debug "!!! symbol 2" (ns-resolve ns symb))
    (debug "!!!------------")
    [(symbol fullpodns (str symb))
     (ns-resolve ns symb)]))

(defn nsmap->lookup
  "Transform the output of ns-map into a map which transforms
  pod.tddpirate.* variables into membrane variables.
  The argument is ns symbol, however."
  [nssym]
  {:pre [(symbol? nssym)]}
  (let [ns (find-ns nssym)]
    (->> (map #(ns+symbol->2tuple "pod.tddpirate." ns %) (-> ns ns-map keys))
         (into {}))))


(def lookup
  "The caller needs to apply var-get to the result of (lookup 'namespace/name)"
  (merge
   { "pod.tddpirate.x" "run*" }
   (nsmap->lookup 'membrane.java2d)
   (nsmap->lookup 'membrane.ui)
   (nsmap->lookup 'membrane.component)
   (nsmap->lookup 'membrane.basic-components)))
(debug "!!! lookup printout --------------------------")
(debug lookup)
(debug "!!! end of lookup printout -------------------")
;; obsolete version
;; {'pod.borkdude.clj-kondo/merge-configs clj-kondo/merge-configs
;;  'clj-kondo.core/merge-configs clj-kondo/merge-configs
;;  'pod.borkdude.clj-kondo/run! clj-kondo/run!
;;  'clj-kondo.core/run! clj-kondo/run!})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a description of a namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nsmap->maps
  "Transform the output of ns-map into a vector whose
  entries are maps from string \"name\" into symbol names."
  [nsmapoutput]
  (mapv #(as-> (% 0) val (name val) {"name" val}) nsmapoutput))

(defn describe-ns
  "Given a namespace, create a namespace description for the
  describe operation.
  podprefix is typically \"pod.tddpirate.\" and is prepended to namespace
  name."
  [podprefix ns]
  {:pre [(string? podprefix)
         (= (last podprefix) \.)
         (= (type ns) clojure.lang.Namespace)]}
  {"name" (->> ns ns-name name (str podprefix))
   "vars" (-> ns
              ns-map
              nsmap->maps)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commented out code

#_(defn pod-ns---example-of-CODE-usge
  "!!! TODO Not actually used, retained to serve as an example."
  [name]
  {"name" name
   "vars" [{"name" "merge-configs"}
           {"name" "print!"
            "code" "
(defn print! [run-output]
  (print (print* run-output))
  (flush))"}
           {"name" "run!"}]})



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main function for running the pod
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-pod []
  (loop []
    (let [message (try (read)
                       (catch java.io.EOFException _
                         ::EOF))]
      (when-not (identical? ::EOF message)
        (let [op (get message "op")
              op (read-string op)
              op (keyword op)
              id (some-> (get message "id")
                         read-string)
              id (or id "unknown")]
          (case op
            :describe (do
                        (debug "===> executing :describe")
                        (write {"format" "edn"
                                "namespaces" [{"name" "pod.tddpirate.x"
                                               "vars" [{"name" "run*"}]}
                                              (describe-ns "pod.tddpirate." (find-ns 'membrane.java2d))
                                              (describe-ns "pod.tddpirate." (find-ns 'membrane.ui))
                                              (describe-ns "pod.tddpirate." (find-ns 'membrane.component))
                                              (describe-ns "pod.tddpirate." (find-ns 'membrane.basic-components))
                                              ]
                                "id" id})
                        (recur))
            :invoke (do (try
                          (let [var (-> (get message "var")
                                        read-string
                                        symbol)
                                args (get message "args")
                                args (read-string args)
                                args (try
                                       (debug "!!! DEBUG membrane.clj: trying to read args, what tag literal is there? args =" args)
                                       (edn/read-string args)
                                       (catch Throwable e
                                         (binding [*out* *err*]
                                           (println e))
                                         (let [reply {"ex-message-edn/read-string" (.getMessage e)
                                                      "ex-data-edn/read-string" (pr-str
                                                                                 (assoc (ex-data e)
                                                                                        :type (class e)))
                                                      "*** args actually read ***" args
                                                      "id" id
                                                      "status" ["done" "error"]}]
                                           (debug "===> executing :invoke/edn/read-string threw an exception")
                                           (write reply)
                                           nil)))
                                ]
                            (if-let [f (var-get (lookup var))]
                              (let [value (pr-str (apply f (deproxify args)))
                                    reply {"value" (proxify value)
                                           "id" id
                                           "status" ["done"]}]
                                (debug "===> executing :invoke execution")
                                (write reply))
                              (throw (ex-info (str "Var not found: " var) {}))))
                          (catch Throwable e
                            (binding [*out* *err*]
                              (println e))
                            (let [reply {"ex-message" (.getMessage e)
                                         "ex-data" (pr-str
                                                    (assoc (ex-data e)
                                                           :type (class e)))
                                         "id" id
                                         "status" ["done" "error"]}]
                              (debug "===> executing :invoke threw an exception")
                              (write reply))))
                        (recur))
            (do
              (debug "===> unknown op:" (name op))
              (write {"err" (str "unknown op:" (name op))})
              (recur))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main
  "Entry point for working as a Babashka pod."
  [ & args ]
  (if (= "true" (System/getenv "BABASHKA_POD"))
    (run-pod)
    (do
      (println "*** NOT OPERATING AS A BABASHKA POD - ABORTING ***")
      (System/exit 1))))
