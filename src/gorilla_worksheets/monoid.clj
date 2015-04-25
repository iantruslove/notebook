;; gorilla-repl.fileformat = 1

;; **
;;; # Commutative Monoid
;;;
;;; These things are neat!
;; **

;; @@
(ns gorilla-worksheets.monoid
  (:require [gorilla-plot.core :as plot]))
;; @@

;; **
;;; Set up an abstraction for working with commutative monoids:
;; **

;; @@
(defprotocol Monoid
  (i [m] "Returns an identity element member of the set.")
  (prepare [m v] "Converts a simple value into an element of the set.")
  (present [m e] "Converts an element of the set to a simple value.")
  (operator
    [m x y]
    "Must be a commutative, associative operation taking two elements
    of the set and returning the combination of input elements, which
    must also be a member of the set."))

;; @@

;; **
;;; Now, a couple of familiar examples. First, the `max` operator:
;; **

;; @@
(def max-monoid
  (reify Monoid
    (i [m] 0)
    (prepare [m v] v)
    (present [m e] e)
    (operator
      [m x y]
      (max x y))))

(let [this-monoid max-monoid]
  (println "Identity:" (i this-monoid))
  (println "Prepare / Present:"
           (prepare this-monoid 10)
           (present this-monoid (prepare this-monoid 20)))
  (println "Single operation: max [10 20] =" (operator this-monoid
                                                       (prepare this-monoid 10)
                                                       (prepare this-monoid 20)))
  (let [xs [4 1 7 5 8 12 4]]
    (println "Reducing a list: "
             "max" xs "="
             (->> xs
                  (map (partial prepare this-monoid))
                  (reduce (partial operator this-monoid))
                  (present this-monoid)))))


;; @@

;; **
;;; More interestingly, the arithmetic average operation:
;; **

;; @@
(def avg-monoid
  (reify Monoid
    (i [m] [0 0])
    (prepare [m v] [v 1])
    (present [m [s n]] (/ s n))
    (operator
      [m x y]
      [(+ (first x) (first y))
       (+ (second x) (second y))])))

(let [this-monoid avg-monoid]
  (println "Identity:" (i this-monoid))
  (println "Prepare / Present:"
           (prepare this-monoid 10)
           (present this-monoid (prepare this-monoid 20)))
  (println "Single operation: avg [10 20] =" (operator this-monoid
                                                       (prepare this-monoid 10)
                                                       (prepare this-monoid 20)))
  (let [xs [4 1 7 5 8 12 4]]
    (println "Reducing a list: "
             "avg" xs "="
             (->> xs
                  (map (partial prepare this-monoid))
                  (reduce (partial operator this-monoid))
                  (present this-monoid)))))
;; @@
