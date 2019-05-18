(ns stigmergy.tily.js
  (:require [clojure.string :as s]
            [clojure.core.async :as a :include-macros true]
            [goog.string :as gstring]
            [goog.string.format]
            [cljsjs.moment]))

(defn to-seq
  "convert a js collection into a ISeq.
  Credit to http://www.dotkam.com/2012/11/23/convert-html5-filelist-to-clojure-vector/"
  [js-col]
  (-> (clj->js [])
      (.-slice)
      (.call js-col)
      (js->clj)))

(defn str->array-buffer [a-str]
  (. (js/TextEncoder. "utf-8") encode a-str))

(defn array-buffer->str [array-buffer]
  (let [decoder (js/TextDecoder. "UTF-8")]
    (.. decoder (decode array-buffer))))

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
    hex-code))

(defn file->text-channel [file]
  (let [c (a/chan 1)
        file-reader (js/FileReader.)]
    (aset file-reader "onload" (fn [evt]
                                 (let [text (.. evt -target -result)]
                                   (a/put! c text)
                                   )))
    (. file-reader (readAsText file))
    c))

(defn file->array-buffer-channel [file]
  (let [c (a/chan 1)
        file-reader (js/FileReader.)]
    (aset file-reader "onload" (fn [evt]
                                 (let [array-buffer (.. evt -target -result)]
                                   (a/put! c array-buffer)
                                   )))
    (. file-reader (readAsArrayBuffer file))
    c))

(defn hash-me
  "hash array-buffer using the given algorithm. algorithm-kw is a keyword of the algorithms supported at 
     https://www.chromium.org/blink/webcrypto. for example :SHA-256"
  [algorithm-kw array-buffer]
  (let [algorithm-str (-> algorithm-kw name s/upper-case)
        c (a/chan 1)]
    (.. (js/crypto.subtle.digest #js{:name algorithm-str} array-buffer)
        (then (fn [the-hash]
                (let [hash-str (s/join "" (array-buffer->hex-str the-hash))]
                  (a/put! c hash-str)))))
    c))

(defn format [& args]
  (apply gstring/format args))

(defn get-dimensions
  ([]
   {:width (. js/window -innerWidth)
    :height  (. js/window -innerHeight)})
  ([element]
   {:width (. element -offsetWidth)
    :height (. element -offsetHeight)}))
