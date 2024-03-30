(ns pod.tddpirate.condwalk
  {:no-doc true}
  (:require [clojure.walk :refer [walk]])
  (:gen-class))

;; This module has two levels of code (in Dijkstra's sense):
;; 1. Lowest level - escape and unescape "complicated" objects.
;; 2. Next level - condwalk, performs conditional walk on a tree.
;; In the future, we'll split this module into two modules.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proxies for objects which cannot be properly
;; serialized and deserialized ("complicated" objects).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Source of inspiration:
;; https://github.com/babashka/babashka-sql-pods/blob/master/src/pod/babashka/sql.clj


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lowest level - identify, escape and unescape "complicated" objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Identify "complicated" objects
(defn simple-obj?
  "Is the argument fully serializable via EDN?"
  [obj]
  (or
   (number? obj)
   (string? obj)
   (char? obj)
   (keyword? obj)
   (symbol? obj)
   (list? obj)
   (vector? obj)
   (set? obj)
   (map? obj)
   (nil? obj)))


;; Escape a "complicated object" by proxying it.
(defn obj->proxy
  "If the object is \"simple\" (fully serializable via EDN), return the object.
  Otherwise, add it to map (if necessary), and return its proxy.
  The arguments are:
  - proxies - a (ref {}) which maps object to string (typically an UUID).
  - revproxies - a (ref {}) which maps string (typically an UUID) to object.
  - UUID-func - creates random strings, typically java.util.UUID/randomUUID
  "
  [proxies revproxies UUID-func obj]
  (if (simple-obj? obj)
    obj
    (if-let [uuidexists (get @proxies obj)] ;; Otherwise, proxy it.
      {::proxy uuidexists}                  ;; object already has a proxy.
      (let [uuid (str (UUID-func))]         ;; Otherwise, create a new proxy.
        (dosync
         (alter proxies assoc obj uuid)
         (alter revproxies assoc uuid obj))
        {::proxy uuid}))))


;; Unescape proxy of a "complicated object"
(defn proxy?*
  "Return a falsey value (nil or false) if the argument is not a proxy.
  Otherwise, return a truthy value which happens to be the proxied object.
  Note that {::proxy false} would be misidentified, but it should never occur
  because false and nil are \"simple\" objects."
  [form]
  (and (map? form) (::proxy form)))

(defn proxy->obj
  "If the argument is a proxy (identified as {::proxy string}),
  convert it back into proxied object.
  Otherwise, return the argument unchanged.
  - revproxies - a (ref {}) which maps string (typically an UUID) to object.
  "
  [revproxies arg]
  (if-let [proxy (proxy?* arg)]
    (get @revproxies proxy)
    arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The condwalk function
;; Conditional walks which handle "complicated" objects and proxies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We want to use combined preorder+postorder tree traversal,
;; because we want to let a node approve/veto traversal of its
;; descendants.
;; Hence, we first visit a node, let it approve/veto, then traverse
;; its descendants, and finally process the node itself.

;; Source: clojure.walk/walk from version 1.1.
(defn condwalk
  "Traverses form, an arbitrary data structure in preorder+postorder.
  approve and func are functions.

  First, apply approve to node.

  If approved, traverse subnodes, using func to transform each of
  them.
  Build data structure of the transformed subnodes, having the same type
  as the form. This data structure will then be transformed by func.

  If approve returned false/nil, apply func only to the whole form without
  recursing into its subnodes.

  Recognizes all Clojure data structures. Consumes seqs as with doall."
  [approve func form]
  (if (approve form)
    (walk (partial condwalk approve func) func form) ;; structurally similar to postwalk.
    (func form)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for use by the rest of the pod code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def proxies (ref {}))    ;; object -> uuid
(def revproxies (ref {})) ;; uuid -> object
(defn UUID-func [] (java.util.UUID/randomUUID)) ;; Redefine in unit tests.


(defn proxify
  "Traverse the argument form and transform any \"complex\" item in it
  into its proxy."
  [form]
  (condwalk simple-obj?
            (partial obj->proxy proxies revproxies UUID-func) ;; obj->proxy invokes simple-obj? too - how to eliminate the double invocation?
            form))

  

(defn deproxify
  "Traverse the argument form and transform any found proxy in it
  into its original form.
  Proxy objects are simple maps, so their contents are not disturbed."
  [form]
  (condwalk proxy?*
            (partial proxy->obj revproxies)
            form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of condwalk.clj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
