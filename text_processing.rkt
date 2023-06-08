;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname homeworkFive) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(require "io-extra.rkt")
;; A [List-of X] is one of
;; - '()
;; - (cons X [List-of X])
;; Interpretation: A list of elements of type X.

;; take-n : Int [List-of X] -> [List-of X]
;; (take-n n alist) produces the first n elements of the list in the order they appear.
;; n must be non-negative, and it produces the whole list if n exceeds the length of the
;; list
(define (take-n n alist)
  (if (>= n (length alist))
      alist
      (cond
        [(= n 0) '()]
        [(> n 0) (cons (first alist) (take-n (- n 1) (rest alist)))])))

(check-expect (take-n 2 (list 1 2 3 4)) (list 1 2))
(check-expect (take-n 2 (list "a" "b" "c")) (list "a" "b"))
(check-expect (take-n 4 (list 1 2 3)) (list 1 2 3))
(check-expect (take-n 0 (list "a" "b" "c" "c")) '())
(check-expect (take-n 1 (list (list 1 2 3) (list "a" "b" "c"))) (list (list 1 2 3)))
(check-expect (take-n 2 (list (list 1 2 3) (list "a" "b" "c")))
              (list (list 1 2 3) (list "a" "b" "c")))

;; drop-n : Int [List-of X] -> [List-of X]
;; (drop-n n alist) produces alist but with the first n elements removed.
;; n must be non-negative, and drop-n produces the empty list if n exceeds the length of the
;; list.

(define (drop-n n alist)
  (if (>= n (length alist))
      '()
      (cond
        [(= n 0) alist]
        [(> n 0) (drop-n (- n 1) (rest alist))])))

(check-expect (drop-n 2 (list 1 2 3 4)) (list 3 4))
(check-expect (drop-n 0 '()) '())
(check-expect (drop-n 1 (list 2)) '())
(check-expect (drop-n 0 (list 2)) (list 2))
(check-expect (drop-n 2 (list "a" "b" "c")) (list "c"))
(check-expect (drop-n 4 (list 1 2 3)) '())
(check-expect (drop-n 0 (list "a" "b" "c" "c")) (list "a" "b" "c" "c"))
(check-expect (drop-n 1 (list (list 1 2 3) (list "a" "b" "c"))) (list (list "a" "b" "c")))
(check-expect (drop-n 2 (list (list 1 2 3) (list "a" "b" "c"))) '())
(check-expect (drop-n 1 (list (list 1))) '())

;; take-while : (X -> Bool) [List-of X] -> [List-of X]
;; (take-while pred alist) produces the longest prefix of alist, where all
;; elements satisfy pred.

(define (take-while pred alist)
  (cond
    [(or (empty? alist) (not (pred (first alist)))) '()]
    [(and (cons? alist) (pred (first alist))) (cons (first alist) (take-while pred (rest alist)))]))

(check-expect (take-while less-than-three? (list 1 2 3 4)) (list 1 2))
(check-expect (take-while less-than-three? (list 3 4)) '())
(check-expect (take-while less-than-three? (list 0 1 2)) (list 0 1 2))
(check-expect (take-while less-than-three? '()) '())
(check-expect (take-while contains-a? (list "racket" "apple" "china" "string"))
              (list "racket" "apple" "china"))
(check-expect (take-while contains-a? (list "string" "racket" "apple" "china")) '())
(check-expect (take-while contains-a? (list "racket" "apple" "china"))
              (list "racket" "apple" "china"))
(check-expect (take-while contains-a? '()) '())
(check-expect (take-while cons? (list (list "dog") (list "dog" "cat") '() (list "raccoon")))
              (list (list "dog") (list "dog" "cat")))
(check-expect (take-while cons? (list '() (list "dog") (list "orangutan"))) '())

;; less-than-three? : Number -> Boolean
;; returns true if the number is less than three, otherwise returns false
(define (less-than-three? n)
  (< n 3))

(check-expect (less-than-three? 3) #f)
(check-expect (less-than-three? 2) #t)
(check-expect (less-than-three? 4) #f)

;; contains-a?: String -> Boolean
;; returns true if the string contains "a", otherwise returns false
(define (contains-a? s)
  (string-contains? "a" s))

(check-expect (contains-a? "racket") #t)
(check-expect (contains-a? "apple") #t)
(check-expect (contains-a? "china") #t)
(check-expect (contains-a? "string") #f)
(check-expect (contains-a? "a") #t)
(check-expect (contains-a? " ") #f)

;; drop-while : (X -> Bool) [List-of X] -> [List-of X]
;; (drop-while pred alist) produces a suffix of alist that we get by dropping
;; the longest prefix of alist that satisfies pred.
(define (drop-while pred alist)
  (cond
    [(or (empty? alist) (not (pred (first alist)))) alist]
    [(and (cons? alist) (pred (first alist))) (drop-while pred (rest alist))]))

(check-expect (drop-while less-than-three? (list 1 2 3 4)) (list 3 4))
(check-expect (drop-while less-than-three? (list 3 4)) (list 3 4))
(check-expect (drop-while less-than-three? (list 0 1 2)) '())
(check-expect (drop-while less-than-three? '()) '())
(check-expect (drop-while contains-a? (list "racket" "apple" "china" "string")) (list "string"))
(check-expect (drop-while contains-a? (list "string" "racket" "apple" "china"))
              (list "string" "racket" "apple" "china"))
(check-expect (drop-while contains-a? (list "racket" "apple" "china")) '())
(check-expect (drop-while contains-a? '()) '())
(check-expect (drop-while cons? (list (list "dog") (list "platypus" "buffalo") '() (list "bison")))
              (list '() (list "bison")))
(check-expect (drop-while empty? (list (list "dog") (list "platypus" "buffalo") '() (list "bison")))
              (list (list "dog") (list "platypus" "buffalo") '() (list "bison")))

;; group-by : (X X -> Boolean) [List-of X] -> [List-of [NE-List-of X]]
;; (group-by same-group? alist) splits alist into groups by comparing
;; consecutive elements to check if they should be in the same group.

(define (group-by same-group? alist)
  (cond
    [(empty? alist) '()]
    [(cons? alist)
     (cons (same-group-list same-group? alist)
           (group-by same-group? (drop-group-list same-group? alist)))]))

(check-expect (group-by = (list 10 10 20 20 20 10)) (list (list 10 10) (list 20 20 20) (list 10)))
(check-expect (group-by < (list 20 30 40 30 50 20 30))
              (list (list 20 30 40) (list 30 50) (list 20 30)))
(check-expect (group-by string<? (list "dog" "buffalo" "cat" "cow" "cow" "animal" "puppy"))
              (list (list "dog") (list "buffalo" "cat" "cow") (list "cow") (list "animal" "puppy")))

(check-expect (group-by > '()) '())

;; same-group-list: (X X -> Boolean) [NE-List-of X] -> [NE-List-of X]
;; (same-group-list same-group? alist) returns a list of the longest prefix of alist
;; such that all elements belong in the same group

(define (same-group-list same-group? alist)
  (cond
    [(or (empty? (rest alist)) (not (same-group? (first alist) (second alist))))
     (cons (first alist) '())]
    [(and (cons? (rest alist)) (same-group? (first alist) (second alist)))
     (cons (first alist) (same-group-list same-group? (rest alist)))]))

(check-expect (same-group-list = (list 10 10 20 20 20 10)) (list 10 10))
(check-expect (same-group-list = (list 20 20 20 10)) (list 20 20 20))
(check-expect (same-group-list = (list 10)) (list 10))
(check-expect (same-group-list < (list 20 30 40 30 50 20 30)) (list 20 30 40))
(check-expect (same-group-list < (list 30 50 20 30)) (list 30 50))
(check-expect (same-group-list < (list 20 30)) (list 20 30))

;; drop-group-list: (X X -> Boolean) [NE-List-of X] -> [List-of X]
;; (drop-group-list same-group? alist) returns a list of the suffix of alist
;; that we get by dropping the longest prefix that belongs in the same group

(define (drop-group-list same-group? alist)
  (cond
    [(or (empty? (rest alist)) (not (same-group? (first alist) (second alist)))) (rest alist)]
    [(and (cons? (rest alist)) (same-group? (first alist) (second alist)))
     (drop-group-list same-group? (rest alist))]))

(check-expect (drop-group-list = (list 10 10 20 20 20 10)) (list 20 20 20 10))
(check-expect (drop-group-list = (list 20 20 20 10)) (list 10))
(check-expect (drop-group-list = (list 10)) '())
(check-expect (drop-group-list < (list 20 30 40 30 50 20 30)) (list 30 50 20 30))
(check-expect (drop-group-list < (list 30 50 20 30)) (list 20 30))
(check-expect (drop-group-list < (list 20 30)) '())

(define ORIG-TEXT (read-lines "pg100.txt"))
(define MOD-TEXT (read-lines "pg100.mod.txt"))
(define MOD-CONTENTS
  (list "THE SONNETS"
        "ALL’S WELL THAT ENDS WELL"
        "THE TRAGEDY OF ANTONY AND CLEOPATRA"
        "AS YOU LIKE IT"))
(define CONTENTS-WO-STUFF (read-lines "contentsWithoutStuff.txt"))

;; remove-whitespace: String -> String
;; Takes a string with some whitespace at the start and returns the string with all of this whitespace removed
(define (remove-whitespace str)
  (cond
    [(= (string-length str) 0) str]
    [(not (string=? " " (substring str 0 1))) str]
    [(string=? " " (substring str 0 1)) (remove-whitespace (substring str 1))]))

(check-expect (remove-whitespace " Apple") "Apple")
(check-expect (remove-whitespace "  Apple") "Apple")
(check-expect (remove-whitespace "  .") ".")
(check-expect (remove-whitespace " ") "")

;; table-of-contents : [List-of String] -> [List-of String]
;; Extracts the table of contents from the lines of text of a book, formatted
;; similarly to pg100.txt.
;; Assumes that the lines do indeed have a table of contents somewhere(it says "Contents" somewhere in some string)
(define (table-of-contents los)
  ;; not-contents? : String -> Boolean
  ;; returns true if the string doesn't contain "Contents", false otherwise
  (local [(define (not-contents? str)
            (not (string-contains? "Contents" str)))
          ;; non-empty? : String -> Boolean
          ;; returns true if the string is a non-empty string, false otherwise
          (define (non-empty? str)
            (not (string=? str "")))]
         (if (= (length (drop-while not-contents? los)) 1)
             '()
             (map remove-whitespace
                  (take-while non-empty? (rest (rest (drop-while not-contents? los))))))))

(check-expect (table-of-contents CONTENTS-WO-STUFF) '())

(check-expect (table-of-contents ORIG-TEXT)
              (list "THE SONNETS"
                    "ALL’S WELL THAT ENDS WELL"
                    "THE TRAGEDY OF ANTONY AND CLEOPATRA"
                    "AS YOU LIKE IT"
                    "THE COMEDY OF ERRORS"
                    "THE TRAGEDY OF CORIOLANUS"
                    "CYMBELINE"
                    "THE TRAGEDY OF HAMLET, PRINCE OF DENMARK"
                    "THE FIRST PART OF KING HENRY THE FOURTH"
                    "THE SECOND PART OF KING HENRY THE FOURTH"
                    "THE LIFE OF KING HENRY THE FIFTH"
                    "THE FIRST PART OF HENRY THE SIXTH"
                    "THE SECOND PART OF KING HENRY THE SIXTH"
                    "THE THIRD PART OF KING HENRY THE SIXTH"
                    "KING HENRY THE EIGHTH"
                    "KING JOHN"
                    "THE TRAGEDY OF JULIUS CAESAR"
                    "THE TRAGEDY OF KING LEAR"
                    "LOVE’S LABOUR’S LOST"
                    "THE TRAGEDY OF MACBETH"
                    "MEASURE FOR MEASURE"
                    "THE MERCHANT OF VENICE"
                    "THE MERRY WIVES OF WINDSOR"
                    "A MIDSUMMER NIGHT’S DREAM"
                    "MUCH ADO ABOUT NOTHING"
                    "THE TRAGEDY OF OTHELLO, MOOR OF VENICE"
                    "PERICLES, PRINCE OF TYRE"
                    "KING RICHARD THE SECOND"
                    "KING RICHARD THE THIRD"
                    "THE TRAGEDY OF ROMEO AND JULIET"
                    "THE TAMING OF THE SHREW"
                    "THE TEMPEST"
                    "THE LIFE OF TIMON OF ATHENS"
                    "THE TRAGEDY OF TITUS ANDRONICUS"
                    "THE HISTORY OF TROILUS AND CRESSIDA"
                    "TWELFTH NIGHT: OR, WHAT YOU WILL"
                    "THE TWO GENTLEMEN OF VERONA"
                    "THE TWO NOBLE KINSMEN"
                    "THE WINTER’S TALE"
                    "A LOVER’S COMPLAINT"
                    "THE PASSIONATE PILGRIM"
                    "THE PHOENIX AND THE TURTLE"
                    "THE RAPE OF LUCRECE"
                    "VENUS AND ADONIS"))

(check-expect (table-of-contents MOD-TEXT)
              (list "THE SONNETS"
                    "ALL’S WELL THAT ENDS WELL"
                    "THE TRAGEDY OF ANTONY AND CLEOPATRA"
                    "AS YOU LIKE IT"))

(define-struct work [title contents])
;; A Work is a (make-work String [List-of String])
;; A (make-work title lines) represents a work of literature with the given
;; title and lines of text.

;; work-template : Work -> ?
(define (work-template w)
  (... (work-title w) ... (work-contents w) ...))

(define EX-WORK-1 (make-work "Hamlet" (list "To be or not to be.")))
(define EX-WORK-2 (make-work "Romeo and Juliet" (list "What's in a name?")))

;; extract-work : String [List-of String]-> Work
;; Consumes the text as a [List-of String] and a title. Returns a work with the given title and extracted content
;; Does this by searching for that title within the text and then extracting all the content
;; including and after the title before the next title occurs.
(define (extract-work title lines)
  ;; not-title? : String -> Boolean
  ;; Consumes a string and returns true if it isn't equal to the title, false otherwise
  (local
   [(define (not-title? str)
      (not (string=? str title)))
    ;; not-next-title? : String -> Boolean
    ;; Returns false if the string is the next title after the title given in extract-work, true otherwise
    (define (not-next-title? str)
      (cond
        [(empty? (rest (drop-while (lambda (x) (not (string=? x title))) (table-of-contents lines))))
         #t]
        [(cons? (rest (drop-while (lambda (x) (not (string=? x title))) (table-of-contents lines))))
         (not (string=? str
                        (second (drop-while (lambda (x) (not (string=? x title)))
                                            (table-of-contents lines)))))]))]
   (make-work title (take-while not-next-title? (drop-while not-title? lines)))))

(check-expect (extract-work "THE SONNETS" MOD-TEXT)
              (make-work "THE SONNETS"
                         (list "THE SONNETS"
                               ""
                               "                    1"
                               ""
                               "From fairest creatures we desire increase,"
                               "That thereby beauty’s rose might never die,"
                               "But as the riper should by time decease,"
                               "His tender heir might bear his memory:"
                               ""
                               "THE END"
                               ""
                               ""
                               "")))

(check-expect (extract-work "ALL’S WELL THAT ENDS WELL" MOD-TEXT)
              (make-work "ALL’S WELL THAT ENDS WELL"
                         (list "ALL’S WELL THAT ENDS WELL"
                               ""
                               "ACT I"
                               ""
                               "SCENE I. Rossillon. A room in the Countess’s palace."
                               ""
                               " Enter Bertram, the Countess of Rossillon, Helena, and Lafew, all in"
                               " black."
                               ""
                               "COUNTESS."
                               "In delivering my son from me, I bury a second husband."
                               ""
                               "[_Exeunt omnes._]"
                               ""
                               ""
                               "")))

(check-expect
 (extract-work "AS YOU LIKE IT" MOD-TEXT)
 (make-work "AS YOU LIKE IT"
            (list "AS YOU LIKE IT"
                  ""
                  "ACT I. SCENE I. Orchard of OLIVER'S house"
                  ""
                  "Enter ORLANDO and ADAM"
                  ""
                  "  ORLANDO. As I remember, Adam, it was upon this fashion bequeathed me"
                  "  by will but poor a thousand crowns, and, as thou say'st, charged my"
                  "  brother, on his blessing, to breed me well; and there begins my"
                  "  sadness. "
                  ""
                  "FINIS"
                  ""
                  ""
                  ""
                  ""
                  "")))

(define SONNETS (extract-work "THE SONNETS" MOD-TEXT))
(define ALL-IS-WELL (extract-work "ALL’S WELL THAT ENDS WELL" MOD-TEXT))
(define AS-YOU-LIKE (extract-work "AS YOU LIKE IT" MOD-TEXT))
(define ANTONY (extract-work "THE TRAGEDY OF ANTONY AND CLEOPATRA" MOD-TEXT))

;; extract-works : [List-of String] [List-of String] -> [List-of Work]
;; (extract-works toc lines) produces the works in a collection, given the list
;; of works extracted from the table of contents. The lines are formatted
;; similar to pg100.txt.
(define (extract-works toc lines)
  (map (lambda (title) (extract-work title lines)) toc))

(check-expect
 (extract-works (list "THE SONNETS" "ALL’S WELL THAT ENDS WELL" "AS YOU LIKE IT") MOD-TEXT)
 (list SONNETS ALL-IS-WELL AS-YOU-LIKE))

(check-expect (extract-works (list "THE SONNETS") MOD-TEXT) (list SONNETS))
(check-expect (extract-works (list "AS YOU LIKE IT") MOD-TEXT) (list AS-YOU-LIKE))

;; extract-works-to-files : String -> [List-of String]
;; Given the path to a collected works, writes several files -- one for each
;; work -- and produces the list of file names.
(define (extract-works-to-files str)
  (map (lambda (w) (write-lines (string-append (work-title w) ".txt") (work-contents w)))
       (extract-works (table-of-contents (read-lines str)) (read-lines str))))

;; top-n-words : Int [List-of String] -> [List-of String]
;; (top-n-words n word-list) produces the n most frequent words in the given
;; word-list
;; Also orders words alphabetically in the case of a tie
(define (top-n-words n los)
  (map (lambda (x) (first x)) (take-n n (length-order (group-by string=? (lower-alph los))))))

(check-expect
 (top-n-words
  3
  (list "apple" "banana" "carrot" "apple" "carrot" "banana" "apple" "banana" "apple" "donut"))
 (list "apple" "banana" "carrot"))

(check-expect (top-n-words 3 '()) '())
(check-expect (top-n-words 0 (list "banana" "banana" "apple")) '())
(check-expect (top-n-words 2 (list "banana" "banana" "apple")) (list "banana" "apple"))
(check-expect (top-n-words 2 (list "banana" "apple")) (list "apple" "banana"))

;; lower-alph: [List-of String] -> [List-of String]
;; converts every string in the list to lowercase and sorts the list alphabetically
(define (lower-alph los)
  (sort (map string-downcase los) string<?))

(check-expect (lower-alph (list "APPLE" "apple" "carrot" "banana" "BANANA"))
              (list "apple" "apple" "banana" "banana" "carrot"))
(check-expect (lower-alph (list "apple" "apple" "banana" "banana" "carrot"))
              (list "apple" "apple" "banana" "banana" "carrot"))

;; length-order: [List-of [List-of X]] -> [List-of [List-of X]]
;; sorts the list in reverse length order.
;; the list of the greatest length is the first in the list,
;; and the list of the smallest length is the last of the list
(define (length-order lolox)
  (sort lolox (lambda (x y) (> (length x) (length y)))))

(check-expect (length-order (list (list 1 2 3) (list 1 2 3 4) (list 1 2)))
              (list (list 1 2 3 4) (list 1 2 3) (list 1 2)))

(check-expect (length-order (list (list "a" "b") (list "a" "b" "c" "d") (list "a" "b" "c")))
              (list (list "a" "b" "c" "d") (list "a" "b" "c") (list "a" "b")))

;; word-frequency : [List-of String] [List-of String] -> [List-of Int]
;; (word-frequency top-words word-list) produces a list that counts
;; the number of occurrences of each word in top-words in word-list.

(define (word-frequency top-words word-list)
  (map (lambda (s) (length (filter (lambda (x) (string=? x s)) (lower-alph word-list))))
       (lower-alph top-words)))

(check-expect (word-frequency (list "apple" "banana" "carrot")
                              (list "banana" "Banana" "Apple" "Carrot" "Apple" "apple"))
              (list 3 2 1))
(check-expect (word-frequency (list "a") (list "a" "b" "c")) (list 1))
(check-expect (word-frequency '() (list "a" "b" "c")) '())
(check-expect (word-frequency (list "a") (list "b" "c")) (list 0))
(check-expect (word-frequency (list "C") (list "c" "C" "c" "C" "C")) (list 5))

;; document-distance : [List-of String] [List-of String] [List-of String] -> Number
;; (document-distance top-words word-list-1 word-list-2) calculates the
;; distance between the word frequency lists of the given files.

(define (document-distance top-words word-list-1 word-list-2)
  (sqrt (foldr +
               0
               (map (lambda (x y) (expt (- y x) 2))
                    (word-frequency top-words word-list-1)
                    (word-frequency top-words word-list-2)))))

(check-expect (document-distance (list "apple" "banana")
                                 (list "apple" "Apple" "banana" "Apple" "Banana")
                                 (list "apple" "banana" "apple" "banana"))
              1)

(check-expect (document-distance (list "apple" "banana")
                                 (list "apple" "Apple" "banana" "Apple" "Banana")
                                 (list "apple" "banana" "apple" "banana" "apple"))
              0)

(check-within (document-distance (list "apple" "banana")
                                 (list "apple" "Apple" "banana" "Apple" "Banana")
                                 (list "apple" "banana" "banana" "banana" "apple"))
              (sqrt 2)
              0)

;; top-n-words/file : Int PathString -> [List-of String]
;; Applies top-n-words to the contents of a file.
(define (top-n-words/file n path)
  (top-n-words n (read-words path)))

;; document-distance/file : [List-of String] PathString PathString -> Number
;; Uses document-distance and word-frequency to calculate the distance between
;; two files.
(define (document-distance/file top-words path-1 path-2)
  (document-distance top-words (read-words path-1) (read-words path-2)))

(define-struct workplace [work distance])
;; A WorkPlace is a (make-workplace Work Number)
;; A (make-workplace work distance) represents a workplace with the given work and the given
;; distance which represents the distance between the work and some work
;; (which is defined when we calculate the distance between some work and all other works)
;; The distance will always be >= 0
;; This distance is defined as Number, but it actually may be a Number or an Inexact Number
;; (depending on the square root)
;; --> There was no other straightforward way to define it, as saying "InexactNumber" wouldn't be any better

;; workplace-template : WorkPlace -> ?
(define (workplace-template wp)
  (... (work-template (workplace-work wp)) ... (workplace-distance wp) ...))

(define EX-WORKPLACE-1 (make-workplace SONNETS 2))
(define EX-WORKPLACE-2 (make-workplace ALL-IS-WELL 0))
(define EX-WORKPLACE-3 (make-workplace ANTONY (sqrt 2)))
;; all-work-distances : Work [List-of Work] -> [List-of WorkPlace]
;; Takes in a work and a list of works and produces a list of all the respective workplaces
;; The list of workplaces contains a workplace for each work, which contains that work
;; and its distance from the work inputted into the function

(define (all-work-distances work work-list)
  (map (lambda (w)
         (make-workplace w
                         (document-distance (top-n-words 45 (work-contents work))
                                            (work-contents work)
                                            (work-contents w))))
       work-list))

(check-expect (all-work-distances SONNETS (list SONNETS AS-YOU-LIKE))
              (list (make-workplace SONNETS 0) (make-workplace AS-YOU-LIKE 4)))
(check-expect (all-work-distances SONNETS (list SONNETS)) (list (make-workplace SONNETS 0)))

;; Cannot check-expect for inexact values, but if we could, a possible test would look like the following:
;; (check-expect (all-work-distances SONNETS (list SONNETS AS-YOU-LIKE ANTONY))
;;               (list (make-workplace SONNETS 0) (make-workplace AS-YOU-LIKE 4)
;;                     (make-workplace ANTONY 2.828)))

;; the map in the final check-expect makes all of the distance values exact
;; so that the check-expect can function and make a comparison

;; We also tested all-work-distances with other values of n other than 45.

;; sort-works : Work [List-of Work] -> [List-of Work]
;; Produces a new list of works which is sorted by similarity to the inputted work.
;; The work itself is the first element of the new list, as it is the most similar to itself

(define (sort-works work work-list)
  (sort
   work-list
   (lambda (x y)
     (<
      (document-distance (top-n-words 40 (work-contents work)) (work-contents work) (work-contents x))
      (document-distance (top-n-words 40 (work-contents work))
                         (work-contents work)
                         (work-contents y))))))

;; We also ran sort-works with several other values of n other than 40.

(check-expect (sort-works SONNETS (list SONNETS SONNETS ALL-IS-WELL SONNETS ALL-IS-WELL SONNETS))
              (list SONNETS SONNETS SONNETS SONNETS ALL-IS-WELL ALL-IS-WELL))

(check-expect (sort-works SONNETS (list ALL-IS-WELL SONNETS ANTONY AS-YOU-LIKE))
              (list SONNETS ANTONY ALL-IS-WELL AS-YOU-LIKE))

(check-expect (sort-works SONNETS (list AS-YOU-LIKE)) (list AS-YOU-LIKE))

(check-expect (sort-works SONNETS '()) '())
