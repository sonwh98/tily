(ns com.kaicode.tily)

(defn with-index
  "return the sequence with an index for every element.
  For example: (with-index [:a :b :c]) returns ([0 :a] [1 :b] [2 :c]).

  The use case for this method ariises with you need access to the index of element of a sequence
  in a for or a doseq"
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
   (defn to-array
     "convert a js collection into a ISeq. Credit to http://www.dotkam.com/2012/11/23/convert-html5-filelist-to-clojure-vector/"
     [js-col]
     (-> (clj->js [])
         (.-slice)
         (.call js-col)
         (js->clj))))
