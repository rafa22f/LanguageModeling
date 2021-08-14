

(define in (open-input-file "Hobbit2.txt"))
;(define in (open-input-file "Alice.txt"))
(define text (read-string 10000 in))
(close-input-port in)

;;;;;;;
;; 2 ;;
;;;;;;;

;;;;;;;;;
;; 2.1 ;;
;;;;;;;;;

;;function that recursively goes through the string replacing before
;;with after
(define (replace str before after)
  (cond [(string=? str "") ""]
        ;;if the first letter of the string is equal to before we append after and then recursively
        ;;call replace on the rest of the string 
        [(string=? (substring str 0 1) before)
         (string-append after(replace (substring str 1) before after))]
        ;;else we append the same letter and then recursively call replace on the rest of the string
        [else (string-append (substring str 0 1)(replace (substring str 1) before after))]))

(replace "pigs are pink" "i" "u")

;;function that replaces newlines, commas and colons with empty string.
(define (strip-punctuation str)
  (replace (replace (replace str "\n" "") "," "") ":" ""))

(define stripped (strip-punctuation text))

;;;;;;;;;
;; 2.2 ;;
;;;;;;;;;

;;function that uses the replace function to add an end of sentence tag
;;after period, question mark and exclamation mark.
(define (add-tags str)
  (replace (replace (replace str "." ". <s>") "?" "? <s>") "!" "! <s>"))

(define tagged (add-tags stripped))

;;;;;;;;;
;; 2.3 ;;
;;;;;;;;;

;;replaces with "" in the string where there is a sep and returns the list of strings
(define (split text sep)
  ;;converts string to list of characters 
  (letrec ((chars (map (lambda (x) (string x)) (string->list text))))
    ;;uses foldl to create a list of results and then reverse to flip that list
    (reverse (foldl (lambda (x r)
                      ;;if current string is equal to sep then break the string by adding space
                      (if (equal? sep x)(cons "" (cons (first r) (rest r)))
                          ;;else append current string to the result
                           (cons (string-append (first r) x) (rest r))))(list "")chars))))
  

(split "hello-world" "-")
(split "the quick brown fox jumped over the lazy dog" "a")

(define (get-words str)
  (split str " "))

(get-words tagged)

(define (filter-empty str)
  (filter (lambda (x) (not (string=? x ""))) str))

(define words (filter-empty (get-words tagged)))

;;;;;;;
;; 3 ;;
;;;;;;;

;;;;;;;;;
;; 3.1 ;;
;;;;;;;;;

;;helper function zip that creates a list of lists with ith elements
(define (zip lst1 lst2)
  ;;returns an empty list if either of the list is empty
  (cond [(or (empty? lst1)(empty? lst2)) '()]
        ;;else recursively creates a list of lists with the ith element
        ;;from both lists
        [else (cons (list (car lst1) (car lst2))
                    (zip (cdr lst1) (cdr lst2)))]))

(zip '(1 2 3) '(4 5 6))

;;;;;;;;;
;; 3.2 ;;
;;;;;;;;;

;;takes a list of words and returns a bigram
(define (make-bigrams words)
  (zip (cons "<s>" words)words))

(define bigrams (make-bigrams words))


;;;;;;;
;; 4 ;;
;;;;;;;

;;;;;;;;;
;; 4.1 ;;
;;;;;;;;;

(define unique-bigrams (remove-duplicates bigrams))
;;creates a list of unique bigrams and a list of forst word in those bigrams
(define matching-unigrams (map (lambda (x) (car x)) unique-bigrams))

;;;;;;;;;
;; 4.2 ;;
;;;;;;;;;

;;function that takes in two lists unique n-grams and all n-grams and returns the count
;;of unique n-grams in the n-grams
(define (get-ngram-counts matching-unigrams words)
  ;;foldl to go through words and increment if the word is present and repeat for all unique ngrams
  (map (lambda (x)(foldl (lambda (y r)
                           (if (equal? x y)(+ 1 r) r))0 words)) matching-unigrams))

(define unique (list "cat" "mat" "sat" "on" "the"))
(define all (list "the" "cat" "sat" "on" "the" "mat" "on" "the" "mat" "sat" "the" "cat"))
(get-ngram-counts unique all)
(define unigram-counts (get-ngram-counts matching-unigrams words))
(define bigram-counts (get-ngram-counts unique-bigrams bigrams))

;;;;;;;;;
;; 4.3 ;;
;;;;;;;;;

;;function that returns a list of bigram probabilities
(define (calc-bigram-probs bigram-counts unigram-counts)
  (map (lambda (x y) (/ x y)) bigram-counts unigram-counts))

(define bigram-probs (calc-bigram-probs bigram-counts unigram-counts))

;;;;;;;
;; 5 ;;
;;;;;;;

;;;;;;;;;
;; 5.1 ;;
;;;;;;;;;

;;function that takes a list of bigrams and heor probabilities and stores in a hashtable
(define (store-probabilities uniques probs)
  (make-immutable-hash (zip uniques probs)))

(define bigram-table (store-probabilities unique-bigrams bigram-probs))
(hash-ref bigram-table '("hobbit" "was"))

;;;;;;;;;
;; 5.2 ;;
;;;;;;;;;

(define (get-relevant-bigrams word unique-bigrams)
  (filter (lambda (x) (string=? (car x) word)) unique-bigrams))

(get-relevant-bigrams "hobbit" unique-bigrams)
  