#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
;(define (cipher-monograms ciphertext)
 ; '())

(define (cipher-monograms ciphertext)

(define (remove l)
  (cond ((null? l) '())
       ( (and (>= (char->integer (car l)) 97) (<= (char->integer (car l)) 122)) (cons (car l) (remove (cdr l))))
       (else (remove (cdr l)))))
  
(define (g l)
  (sort (freq l) (lambda (x y) (>= (cdr x) (cdr y)))))
  
(define (check2 a l)
  (check3 a l 0))

(define (check3 a l n)
    (cond ((null? l) n)
        ((equal? (car l) a) (check3 a (cdr l) (+ n 1)))
        (else (check3 a (cdr l) n))))

(define (freq l)
  (if (null? l) '()
  (remove-duplicates (map (lambda (a) (cons a (check2 a l))) l))))
  
(define (remove-duplicates l)
  (fun l (cons (car l) '())))

(define (check a l)
  (cond ((null?  l) #f)
        ((equal? a (car l)) #t)
        (else (check a (cdr l)))))
  
(define (fun l p)
  (cond ((null?  l) p)
        ((check (car l) p)(fun (cdr l) p))
         
        (else (fun (cdr l) (append p (cons (car l) '())) ))))
        
(remove (map (lambda (x) (car x)) (g (string->list ciphertext)))))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
;(define (cipher-bigrams cipher-word-list)
 ; '())

(define (cipher-bigrams cipher-word-list)
  
(define (check2 a l)
  (check3 a l 0))

(define (check3 a l n)
    (cond ((null? l) n)
        ((equal? (car l) a) (check3 a (cdr l) (+ n 1)))
        (else (check3 a (cdr l) n))))

(define (freq l)
  (if (null? l) '()
  (remove-duplicates (map (lambda (a) (cons a (check2 a l))) l))))

(define (fun l)
  (map (lambda (x) (string->list x)) l))

(define (combine l)
  (if (null? (cdr l)) '()
      (append (list (cons (car l) (cadr l))) (combine (cdr l)))))

(define (str-to-pair l)
  (append*(map (lambda (x) (combine x)) (fun l))))

(define (f l)
  (freq (str-to-pair l)))

(map (lambda (l) (list->string (list (car l) (cdr l)))) (map (lambda (x) (car x) ) (sort (f cipher-word-list) (lambda (x y) (>= (cdr x) (cdr y)))))))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
;(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
 ; '())

(define (cipher-unique-neighbourhood cipher-bigrams-list mode )

  
  (define (check2 a l)
  (check3 a l 0))

  (define (check3 a l n)
    (cond ((null? l) n)
        ((equal? (car l) a) (check3 a (cdr l) (+ n 1)))
        (else (check3 a (cdr l) n))))

(define (freq l)
  (if (null? l) '()
  (remove-duplicates (map (lambda (a) (cons a (check2 a l))) l))))


  (define (combine l)
  (if (null? (cdr l)) '()
      (append (list (cons (car l) (cadr l))) (combine (cdr l)))))
(define (fun l)
  (map (lambda (x) (string->list x)) l))
  
(define (str-to-pair l)
  (append*(map (lambda (x) (combine x)) (fun l))))

(define (f l)
  (freq (str-to-pair l)))

(define (countb a l)
  (count1 a l 0))

(define (count1 a l n)
  (cond ((null? l) n)
         (else (if (or (equal? a (caar l)) (equal? a (cdar l))) (count1 a (cdr l) (+ n 1))
             (count1 a (cdr l) n)))))
  (define (countp a l)
  (count2 a l 0))

  (define (count2 a l n)
  (cond ((null? l) n)
         (else (if  (equal? a (caar l))  (count2 a (cdr l) (+ n 1))
             (count2 a (cdr l) n)))))
  (define (count-s a l)
  (count3 a l 0))
  (define (count3 a l n)
  (cond ((null? l) n)
         (else (if  (equal? a (cadr l))  (count3 a (cdr l) (+ n 1))
             (count3 a (cdr l) n)))))

(define alpha '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

  (cond ((equal? mode 'predecessor)
         (sort (map (lambda (x) (cons x (countp x (str-to-pair cipher-bigrams-list)))) alpha) (lambda (x y) (>= (cdr x) (cdr y)))))
       ( (equal? mode 'successor)
         (sort (map (lambda (x) (cons x (count-s x (str-to-pair cipher-bigrams-list)))) alpha) (lambda (x y) (>= (cdr x) (cdr y)))))
       ((equal? mode 'both)
       (sort (map (lambda (x) (cons x (countb x (str-to-pair cipher-bigrams-list)))) alpha) (lambda (x y) (>= (cdr x) (cdr y)))))))
;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
;(define (cipher-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
 ; '())

(define (cipher-neighbourhood cipher-bigrams-list mode )  
  (define (check2 a l)
  (check3 a l 0))
  
  (define (check3 a l n)
    (cond ((null? l) n)
        ((equal? (car l) a) (check3 a (cdr l) (+ n 1)))
        (else (check3 a (cdr l) n))))

(define (freq l)
  (if (null? l) '()
  (remove-duplicates (map (lambda (a) (cons a (check2 a l))) l))))
  
  (define (combine l)
  (if (null? (cdr l)) '()
      (append (list (cons (car l) (cadr l))) (combine (cdr l)))))
(define (fun l)
  (map (lambda (x) (string->list x)) l))
  
(define (str-to-pair l)
  (append*(map (lambda (x) (combine x)) (fun l))))

(define (f l)
  (freq (str-to-pair l)))

(define (countb a l)
  (count1 a l 0))

(define (count1 a l n)
  (cond ((null? l) n)
         (else (if (or (equal? a (caar l)) (equal? a (cdar l))) (count1 a (cdr l) (+ n 1))
             (count1 a (cdr l) n)))))
  (define (countp a l)
  (count2 a l 0))

  (define (count2 a l n)
  (cond ((null? l) n)
         (else (if  (equal? a (caar l))  (count2 a (cdr l) (+ n 1))
             (count2 a (cdr l) n)))))
  (define (count-s a l)
  (count3 a l 0))
  (define (count3 a l n)
  (cond ((null? l) n)
         (else (if  (equal? a (cadr l))  (count3 a (cdr l) (+ n 1))
             (count3 a (cdr l) n)))))

(define alpha '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

  (cond ((equal? mode 'predecessor)
         (sort (map (lambda (x) (cons x (countp x (str-to-pair cipher-bigrams-list)))) alpha) (lambda (x y) (>= (cdr x) (cdr y)))))
       ( (equal? mode 'successor)
         (sort (map (lambda (x) (cons x (count-s x (str-to-pair cipher-bigrams-list)))) alpha) (lambda (x y) (>= (cdr x) (cdr y)))))
       ((equal? mode 'both)
       (sort (map (lambda (x) (cons x (countb x (str-to-pair cipher-bigrams-list)))) alpha) (lambda (x y) (>= (cdr x) (cdr y)))))))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  '())

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
;(define (cipher-common-words-single cipher-word-list)
 ;
;'())

(define (cipher-common-words-single cipher-word-list)
  (define l1 cipher-word-list)

(define (convert l) (map (lambda (x) (string->list x)) l))

(define (single lst)
  (if (null?  lst) '()
   (if (= (length (car  lst)) 1) (cons (caar  lst) (single (cdr  lst)))
       (single (cdr  lst)))))

 (define (check2 a l)
  (check3 a l 0))

  (define (check3 a l n)
    (cond ((null? l) n)
        ((equal? (car l) a) (check3 a (cdr l) (+ n 1)))
        (else (check3 a (cdr l) n))))
  (if (> (check2 (car (remove-duplicates (single (convert l1)))) (single (convert l1))) (check2 (cadr (remove-duplicates (single (convert l1))))
                                                                                              (single (convert l1))))
      (map (lambda (x) (list->string (list x)))  (remove-duplicates (single (convert l1))))
      (map (lambda (x) (list->string (list x)))  (reverse (remove-duplicates (single (convert l1)))))))
;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
