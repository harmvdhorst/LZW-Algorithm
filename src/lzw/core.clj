(ns lzw.core
  ;; Here I import the string namespace of clojure used to split the string
  (:require [clojure.string :as str]))

;; here I create the default dictionary for the algorithm
(defn create-default-dict []
  {:A 1, :B 2, :C 3, :D 4, :E 5, :F 6, :G 7, :H 8, :I 9, :J 10,
    :K 11, :L 12, :M 13, :N 14, :O 15, :P 16, :Q 17, :R 18, :S 19, :T 20,
    :U 21, :V 22, :W 23, :X 24, :Y 25, :Z 26, " " 27, "." 28, "," 29, "?" 30, "!" 31})

(defn compress [s]
      ;; split the string into a vector
      (let [chars (str/split s #"")]
            ;; create the master loop which holds the current vector which shrinks every loop
            ;; this also holds the dictionary.
           (loop [data chars, dict (create-default-dict)]
             ;; This makes it so the loop stops when the vector is empty
             (when (seq data)
               ;; get the first char in the vector
               (let [current (first data)]
                 ;; create the second loop + state for the extra chars
                 ;; state for the chars of the second loop minus the current char
                 (loop [d (rest chars), extra ""]
                   ;; continue running until the current char + the extra chars do not exist in the dictionary
                   (when (not (contains? dict (str current extra)))
                     ;; get the first char
                     (let [currentExtraLetter (first d)]
                     ;; change the state so that the chars are updated + add the newly found letter to the extra string for next iteration
                     (recur (rest d) (str extra currentExtraLetter)))))
           ;; update the state with the rest of the vector minus the first one
           (recur (rest data) (assoc dict :c 1)))))))

(defn compressChar [chars, dict]
  ;; first we fetch the char we are currently encoding
  (let [current (first chars)]
    ;; then we make a string for the extra chars + make a state for the rest of the chars
    (loop [extraString "", extraChars (rest chars)]
      ;; we need to make a failsafe for and empty chars vector
      (when (seq extraChars)
        ;; now we check if our new string is in the dict
       (if (not (contains? dict (str current extraString)))
         ;; we found a char combination which does not already exist in the dictionary
         ;; no we find the next free index for our newly found
         (let [nextIndex (inc (apply max (vals dict)))]
           ;; we get the last string combo for return value
           (let [prevString (apply str (drop-last extraString))]
           ({
             ;; we add the new string to dict
             :dict (assoc dict extraString nextIndex),
             ;; create the output
             :output (get dict prevString)
             })))
         ;; already exists so we add the next char and try again
         (recur (str extraString (first extraChars)) (rest extraChars)))))))


;; Here I run the main function with some really simple text to trigger the compression
(compress "ABAAABBACABABCADD")