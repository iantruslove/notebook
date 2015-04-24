;; gorilla-repl.fileformat = 1

;; **
;;; # Trending
;;;
;;; `<shift>+<enter>` evaluates code.
;;;
;;; Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands...
;;;
;; **

;; **
;;; ## TODO
;;;
;;; * Commutative monoids for data processing
;; **

;; **
;;; Set up the ns and define useful functions:
;; **

;; @@
(ns gorilla-worksheets.trending
  (:require [gorilla-plot.core :as plot]))

(defn separate-pairs
  "For a list of [[x y] ...], return the separated abscissa and
  ordinates [[xs ...] [ys ...]]."
  [pairs]
  [(map first pairs) (map second pairs)])

(defn into-pairs
  "The inverse of `separate-pairs`."
  [[xs ys]]
  (map vector xs ys))

(defn vals-to-pairs
  ([vals]
   (vals-to-pairs vals 0))
  ([vals start-x]
   (vals-to-pairs vals start-x 1))
  ([vals start-x inc-x]
   (into-pairs [(iterate (partial + inc-x) start-x) vals])))

(defn average [vals]
  (/ (reduce + vals) (count vals)))

(defn pairs-average [pairs]
  (let [[sum n] (reduce (fn [[sum n] [x y]]
                          [(+ sum y) (inc n)])
                        [0 0] pairs)]
    (/ sum n)))

(defn smooth-trailing-three [vals]
  (conj
   (map (fn [triple]
          (/ (reduce + triple) 3))
        (partition 3 1 vals))
   nil nil))

(defn pairs-map [f pairs & colls]
  (let [[xs ys] (separate-pairs pairs)]
    (into-pairs [xs (apply map f ys colls)])))

;; @@

;; **
;;; Set up some data
;; **

;; @@
(def values [5 7 6 7 4 7 6 5 6 10 5 8 3 6 5 4 7 5 5 7 10
             12 13 17 26 28 39 43 41 42 36 32 26 20 19 20
             15 14 12 9 5 2 8 7 5 6 7 4 2 1 0 3 0 0 2 1
             0 0 2 1 2 0 2 3 6 4 7 8 5 6 6 7 7 10 8 3])

;; This would be stored, not calculated...
(def long-term-average (average (take 20 values)))

(def time-series (vals-to-pairs values))

(defn scale [factor n]
  (/ n factor))

(defn log-norm [norm-factor val]
  (->> val
       (scale norm-factor)
       inc ;; to avoid trying to take the log of 0
       Math/log))

(def normalized-series (pairs-map
                        (partial log-norm long-term-average)
                        time-series))

(def normalized-series-plot (plot/list-plot
                             normalized-series
                             :joined true))

(def normalized-series-average-plot
  (plot/plot (constantly (log-norm long-term-average long-term-average))
             [0 (dec (count values))]
             :color :black
             :opacity 0.1))

(defn d
  "Discrete derivative of the pairs collection"
  [pairs]
  (let [[_ ys] (separate-pairs pairs)]
    (pairs-map - pairs (cons 0 ys))))

(def deltas (d normalized-series))

;; TODO: smooth the deltas

;; @@

;; @@
"Original Data:"
(plot/compose
 (plot/list-plot values :joined true)
 (plot/plot (constantly long-term-average)
            [0 (dec (count values))]
            :color :black
            :opacity 0.1))

"Log-Normalized Values:"
(plot/compose
 normalized-series-plot
 normalized-series-average-plot)

;; @@

;; **
;;; Trending / smoothing
;; **

;; @@
(defn decay-trailing-n
  ([pairs n]
   (decay-trailing-n pairs n 0.5))
  ([pairs n factor]
   (let [[xs ys] (separate-pairs pairs)]
     (into-pairs
      [xs
       (apply conj
              (map (fn [triple]
                     (reduce (fn [acc val] (+ val (* factor acc))) triple))
                   (partition n 1 ys))
              (repeat n nil))]))))

(defn only-abs-above* [threshold val]
  (when (not (nil? val))
    (when (> (Math/abs val) threshold)
      val)))

(defn only-abs-above [vals threshold]
  (map (partial only-abs-above* threshold) vals))

(def trend-vals (decay-trailing-n deltas 8 0.8))

"Trend:" (plot/compose
          (plot/list-plot trend-vals
                          :joined true :color :blue)
          #_(plot/list-plot (only-abs-above trend-vals 0.3)
                            :joined false :color :red)
          (plot/list-plot deltas
                          :joined true :color :orange
                          :opacity 0.5))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gorilla-worksheets.trending/only-abs-above</span>","value":"#'gorilla-worksheets.trending/only-abs-above"}
;; <=

;; @@

;; @@
