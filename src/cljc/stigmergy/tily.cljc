(ns stigmergy.tily
  (:refer-clojure :exclude [format])
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.set]
            #?(:cljs [cljsjs.moment])
            #?(:cljs [goog.string :as gstring])
            ))

;;miscellaneous utility functions

(defn with-index
  "return the sequence with an index for every element.
  For example: (with-index [:a :b :c]) returns ([0 :a] [1 :b] [2 :c]).

  The use case for this method arises when you need access to the index of element of a sequence
  For example:

  (doseq [[index element] (with-index [:a :b :c :d])]
    (println index element)"
  [a-seq]
  (map-indexed (fn [i element] [i element])
               a-seq))

(defn drop-nth [v n]
  (let [after (+ n 1)
        part1 (subvec v 0 n)
        part2 (subvec v after (count v))]
    (vec (concat part1 part2))))

(defn insert-at [v n val]
  (let [before-n (subvec v 0 n)
        after-n (subvec v n (count v))]
    (vec (concat before-n [val] after-n))))

(defn is-contained?
  "Example: (is-contained? 1 :in [1 2 3])"
  [a _ b]
  (not (nil? (some #(= a %)  b))))

(defn some-in?
  "returns true if there some element in a also exists in b"
  [a b]
  (let [a (if (coll? a)
            (set a)
            (hash-set a))
        b (if (coll? b)
            (set b)
            (hash-set b))]
    (-> a (clojure.set/intersection b)
        empty? not)))

(defn remove-nils [m]
  (let [rm-nil-val (fn [[k v]] (when v [k v]))]
    (w/prewalk (fn [x]
                 (cond
                   (map? x) (into {} (map rm-nil-val x))
                   (vector? x) (vec (remove nil? x))
                   :else x))
               m)))

(defn str->date [date-as-str]
  #?(:cljs (let [m (-> date-as-str (js/moment. "MM/DD/YYYY"))]
             (. m toDate))))

(defn date->str [date]
  #?(:cljs (.. (js/moment. date) (format "MM/DD/YYYY"))))

(defn normalize-str [a-str]
  (-> a-str (or "")
      clojure.string/trim
      clojure.string/lower-case))

(defn set-atom! [an-atom path val]
  (swap! an-atom update-in path (constantly val)))

(defn format [fmt & args]
  #?(:cljs
     (apply gstring/format fmt args)))

(defn suck
  "like slurp but returns byte-array"
  [file-name]
  #?(:clj
     (let [paths (clojure.string/split file-name #"/")
           root-dir (let [fp (first paths)]
                      (if (= "" fp)
                        "/"
                        fp))
           path (java.nio.file.Paths/get root-dir (into-array (rest paths)))]
       (try
         (java.nio.file.Files/readAllBytes path)
         (catch Exception ex nil)))))

(defn take-between [i j coll]
  (let [chunk (drop i coll)
        num (- j i)]
    (take num chunk)))
