;; gorilla-repl.fileformat = 1

;; **
;;; # Term Frequencies with Count Min Sketch
;;; 
;;; It occurred to me that count min sketches (CMS) (with identical parameters) are combineable. A sketch for a document could encode the TF distribution for the doc, and the combination of more than one document's sketches would be the TF distribution for that set of documents.
;;; 
;;; If documents have their CMSs calculated at analysis time, then the daily TF would just be the combination of those CMSs... Easy!
;;; 
;;; ## Things to figure out:
;;; 
;;; * How do CMSs aggregate?
;;; * What's an efficient way to figure out what terms are in a given email (if that even needs doing)?
;;; * How can a series of CMSs be turned into what I need for time series data for trending?
;;; 
;; **

;; @@
(ns gorilla-worksheets.count-min-sketch
  (:require [gorilla-plot.core :as plot]
            [bigml.sketchy.count-min :as count-min]
            [clojure.string :as string]
            [doric.core :as doric]))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Set up some data:
;; **

;; @@
(def doc-text (str "this is a great document it has all kinds "
                   "of words and no punctuation so it can be "
                   "split just on the space character "
                   "it is also a good piece of text because "
                   "it has the best words in any string"))

(defn tokenize [s]
  (string/split s #" "))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gorilla-worksheets.count-min-sketch/tokenize</span>","value":"#'gorilla-worksheets.count-min-sketch/tokenize"}
;; <=

;; **
;;; Set up a count-min sketch for this document:
;; **

;; @@
(def cm (count-min/into (count-min/create :hash-bits 9)
                        (tokenize doc-text)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gorilla-worksheets.count-min-sketch/cm</span>","value":"#'gorilla-worksheets.count-min-sketch/cm"}
;; <=

;; **
;;; What are the differences between the actual counts and the estimated counts from the CMS? If I tweak the sketch's `:hash-bits` and `:hashers` parameters, obviously the collision (and error) rates change...
;; **

;; @@
(let [tokens (tokenize doc-text)
      freqs (frequencies tokens)
      terms (keys freqs)
      cm (count-min/into (count-min/create :hash-bits 6 :hashers 3)
                         tokens)
      counts (->> (for [term terms]
                    {:term term
                     :estimated (count-min/estimate-count cm term)
                     :actual (freqs term)})
                  (map (fn [m]
                         (assoc m
                           :delta (- (:estimated m) (:actual m))))))
      sorted-counts (sort (fn [x y]
                            (> (:delta x) (:delta y)))
                          counts)]
  (println (doric/table [:term :estimated :actual :delta]
                        sorted-counts)))
;; @@
;; ->
;;; |-------------+-----------+--------+-------|
;;; |     Term    | Estimated | Actual | Delta |
;;; |-------------+-----------+--------+-------|
;;; | has         | 3         | 2      | 1     |
;;; | space       | 1         | 1      | 0     |
;;; | punctuation | 1         | 1      | 0     |
;;; | string      | 1         | 1      | 0     |
;;; | of          | 2         | 2      | 0     |
;;; | this        | 1         | 1      | 0     |
;;; | it          | 4         | 4      | 0     |
;;; | also        | 1         | 1      | 0     |
;;; | is          | 2         | 2      | 0     |
;;; | good        | 1         | 1      | 0     |
;;; | just        | 1         | 1      | 0     |
;;; | words       | 2         | 2      | 0     |
;;; | great       | 1         | 1      | 0     |
;;; | document    | 1         | 1      | 0     |
;;; | because     | 1         | 1      | 0     |
;;; | any         | 1         | 1      | 0     |
;;; | can         | 1         | 1      | 0     |
;;; | character   | 1         | 1      | 0     |
;;; | text        | 1         | 1      | 0     |
;;; | split       | 1         | 1      | 0     |
;;; | a           | 2         | 2      | 0     |
;;; | so          | 1         | 1      | 0     |
;;; | on          | 1         | 1      | 0     |
;;; | be          | 1         | 1      | 0     |
;;; | kinds       | 1         | 1      | 0     |
;;; | and         | 1         | 1      | 0     |
;;; | piece       | 1         | 1      | 0     |
;;; | no          | 1         | 1      | 0     |
;;; | all         | 1         | 1      | 0     |
;;; | best        | 1         | 1      | 0     |
;;; | the         | 2         | 2      | 0     |
;;; | in          | 1         | 1      | 0     |
;;; |-------------+-----------+--------+-------|
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; 
;; **

;; @@

;; @@
