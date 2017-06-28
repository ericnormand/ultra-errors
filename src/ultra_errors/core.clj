(ns ultra-errors.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn arg-error [cn & args]
  (let [e (IllegalArgumentException.
           (apply str "\n\n" (str/triml (str (first args))) (rest args)))]
    (comment
      (.setStackTrace
       (into-array StackTraceElement
                   (->> (.getStackTrace e)
                        (drop-while #(not= cn (.getClassName %)))
                        (rest)))))
    (def j e)
    (throw e)))

(def replacements
  {'clojure.core
   {'map {:validate (fn [cn [f & lists]]
                      (when-not (ifn? f)
                        (arg-error cn "

The first argument to clojure.core/map has an unexpected type.

It should be a function or something callable as a function (such as a
keyword). However, the first argument has " (class f) ".

Here's the value of f:

" (binding [*print-length* 10
            *print-level* 2]
    (with-out-str (pprint/pprint f))) "

")))
          :wrap (fn [cn f] (f))}}})

(comment
  (defn replace-map []
    (let [v (var clojure.core/map)]
      (let [map (get (meta v) :ultra-errors/old-def @v)]
        (alter-meta! v assoc :ultra-errors/old-def map)
        (alter-var-root v
                        (fn [_]
                          (fn [& args]
                            (try (assert (ifn? f))
                                 (catch Throwable t
                                   (def j t)
                                   (throw t)))
                            (try
                              (map f ls)
                              (catch Exception e
                                (throw e))))))))))

(defn replace-all []
  (doseq [[ns vars] replacements]
    (when (try
            (require ns)
            :success
            (catch Exception e
              nil))
      (doseq [[var {:keys [validate wrap]
                    :or   {validate (fn [cn _])
                           wrap     (fn [cn f] (f))}}] vars]
        (when-let [v (ns-resolve *ns* var)]
          (let [orig-fn (get (meta v) :ultra-errors/old-def @v)]
            (alter-meta! v assoc :ultra-errors/old-def orig-fn)
            (alter-var-root
             v
             (fn [_]
               (let [cn "oo"]
                (fn [args]
                  (validate cn args)
                  (wrap cn #(apply orig-fn args))))))))))))
