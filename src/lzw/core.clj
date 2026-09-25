(ns lzw.core
  ;; Here I import the string namespace of clojure used to split the string
  (:require [clojure.string :as str]))

;; here I create the default dictionary for the algorithm
(defn create-default-dict []
  {"A" 1, "B" 2, "C" 3, "D" 4, "E" 5, "F" 6,
   "G" 7, "H" 8, "I" 9, "J" 10, "K" 11, "L" 12,
   "M" 13, "N" 14, "O" 15, "P" 16, "Q" 17, "R" 18,
   "S" 19, "T" 20, "U" 21, "V" 22, "W" 23, "X" 24,
   "Y" 25, "Z" 26,
   " " 27, "." 28, "," 29, "?" 30, "!" 31})

(defn compressChar [chars, dict]
  ;; first we fetch the char we are currently encoding
  (let [current (first chars)]
    ;; then we make a string for the extra chars + make a state for the rest of the chars
    (loop [currentCombo current, extraChars (rest chars)]
      ;; we need to make a failsafe for and empty chars vector
      ;; if
      (if (seq extraChars)
        ;; we store the old combo + the next char and store this in newCombo
        (let [newCombo (str currentCombo (first extraChars))]
          ;; now we check if our new string is in the dict
          (if (not (contains? dict newCombo))
            ;; we found a char combination which does not already exist in the dictionary
            ;; now we find the next free index for our newly found
            (let [nextIndex (inc (apply max (vals dict)))]
              ;; we get the last string combo for return value
              (let [prevString (apply str (drop-last newCombo))] {
                ;; we add the new string to dict
                :dict (assoc dict newCombo nextIndex),
                ;; create the output
                :output (get dict prevString)
                ;; return the chars we did not need
                :remaining_chars (drop (count prevString) chars)
               }))
            ;; already exists so we recur back to the beginning with our newest longest combo and the rest of the chars
            (recur newCombo (rest extraChars))))
        ;; else
        ;; when the sequence is finished we return the current, dict and the output of our current longest combo
      {
       :dict dict
       :output (get dict currentCombo)
       :remaining_chars []
       }))))

(defn compress [s]
  ;; split the string into a vector, also we make it uppercase so when can use a and A in the compression data
  (let [chars (str/split (str/upper-case s) #"")]
    ;; create the master loop which holds the current vector which shrinks every loop
    ;; this also holds the dictionary.
    ;; finally we store the output so when can log it in the end
    (loop [data chars, dict (create-default-dict), output ""]
      ;; check if the sequence is empty
      (if (seq data)
        ;; use our compressChar function, with our data and premade dict
        (let [result (compressChar data, dict)]
          ;; now we recur with our returned data
          (recur
            ;; we get our remaining chars
            (get result :remaining_chars)
            ;; we get our most recent dict
            (get result :dict)
            ;; and we append the output number to our output
            (str output " " (get result :output))
            )
          )
        ;; and last but not least, we will return the output for the println
        output
        ))))




;; Here I run the main function with some really simple text to trigger the compression
;; test data generated using AI
(println (compress "DIT IS EEN TEST. DIT IS EEN LANGERE TEST. DEZE TEST TEST OF DE COMPRESSIE WERKT. DIT IS EEN TEST MET VEEL HERHALING. DE COMPRESSIE MOET DE HERHALING HERKENNEN. DIT IS EEN LANGERE TEST. DEZE TEST TEST OPNIEUW OF DE COMPRESSIE WERKT. HALLO HALLO HALLO! DIT IS EEN TEST. HALLO DIT IS EEN TEST! DE COMPRESSIE WERKT ALS DEZELFDE WOORDEN EN ZINNEN STEEDS OPNIEUW VOORKOMEN. TEST TEST TEST. DIT IS EEN LANGERE TEST. HALLO HALLO! DEZE TEST IS BIJNA KLAAR. DIT IS EEN TEST. DIT IS EEN TEST. DIT IS EEN TEST!"))