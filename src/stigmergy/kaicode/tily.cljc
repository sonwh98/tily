(ns com.kaicode.tily
  (:require [clojure.string :as s]
            #?(:cljs [cljs.core.async :refer [<! put! chan]])
            [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.set]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format])
            #?(:cljs [cljsjs.moment])))

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

#?(:cljs
   (defn to-seq
     "convert a js collection into a ISeq. Credit to http://www.dotkam.com/2012/11/23/convert-html5-filelist-to-clojure-vector/"
     [js-col]
     (-> (clj->js [])
         (.-slice)
         (.call js-col)
         (js->clj))))


#?(:cljs
   (defn debug
     "println to javascript console. using boot.cljs causes println to send output to the repl and not to the js console"
     [& edn]
     (let [output (str edn)
           output-unwrapped (subs output 1 (-> output count dec))]
       (js/console.log output-unwrapped))))

#?(:cljs
   (defn str->array-buffer [a-str]
     (. (js/TextEncoder. "utf-8") encode a-str)))

#?(:cljs
   (defn array-buffer->hex-str [array-buffer]
     (let [view (js/DataView. array-buffer)
           length (.. view -byteLength)
           hex-code (clj->js (for [i (range 0 length 4)
                                   :let [value (. view getUint32 i)
                                         string-value (. value toString 16)
                                         padding "00000000"
                                         padding-value (. (str padding string-value)
                                                          slice (* -1 (. padding -length)))]]
                               padding-value))]
       hex-code)))

#?(:cljs
   (defn file->text-channel [file]
     (let [c (chan 1)
           file-reader (js/FileReader.)]
       (aset file-reader "onload" (fn [evt]
                                    (let [text (.. evt -target -result)]
                                      (put! c text)
                                      )))
       (. file-reader (readAsText file))
       c)))


#?(:cljs
   (defn file->array-buffer-channel [file]
     (let [c (chan 1)
           file-reader (js/FileReader.)]
       (aset file-reader "onload" (fn [evt]
                                    (let [array-buffer (.. evt -target -result)]
                                      (put! c array-buffer)
                                      )))
       (. file-reader (readAsArrayBuffer file))
       c)))

#?(:cljs
   (defn hash-me
     "hash array-buffer using the given algorithm. algorithm-kw is a keyword of the algorithms supported at 
     https://www.chromium.org/blink/webcrypto. for example :SHA-256"
     [algorithm-kw array-buffer]
     (let [algorithm-str (-> algorithm-kw name s/upper-case)
           c (chan 1)]
       (.. (js/crypto.subtle.digest #js{:name algorithm-str} array-buffer)
           (then (fn [the-hash]
                   (let [hash-str (s/join "" (array-buffer->hex-str the-hash))]
                     (put! c hash-str)))))
       c)))


#?(:cljs 
   (defn format [& args]
     (apply gstring/format args)))

(defn is-contained?
  "Example: (is-contained? 1 :in [1 2 3])"
  [a _ b]
  (not (nil? (some #(= a %)  b))))

(defn some-in?
  "returns true if there some element in a also exists in b"
  [a b]
  (-> a (clojure.set/intersection b)
      empty? not))

(defn remove-nils
  [m]
  (let [f (fn [[k v]] (when v [k v]))]
    (w/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

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

#?(:cljs
   (defn get-dimensions
     ([]
      {:width (. js/window -innerWidth)
       :height  (. js/window -innerHeight)})
     ([element]
      {:width (. element -offsetWidth)
       :height (. element -offsetHeight)})))
