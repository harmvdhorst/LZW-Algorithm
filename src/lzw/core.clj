(ns lzw.core
  ;; Here I import the string namespace of clojure used to split the string
  (:require [clojure.string :as str]))

;; Here I declare the compression main method
(defn compress [data]
  ;; Here I split the data string into a vector, so I easily loop through all the characters
  (let [chars (str/split data #"")]
    ;; I need to have an index so I know what position I'm at
    (loop [i 0]
      ;; Here is the stop condition which stops when the characters are all done
      (when (< i (count chars))
        ;; Here I fetch the current character using the I from the loop
        (let [j (chars i)]
          (println j)
          ;; After that I use recur + inc to increment the index and start the loop all over again for the next character
          (recur (inc i)))))))


;; Here I run the main function with some really simple text to trigger the compression
(compress "ABAAABBACABABCADD")