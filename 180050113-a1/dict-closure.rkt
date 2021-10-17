#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (dictionary-closure key)
 ; key)

(define (dictionary-closure key)
  
  
  (let ((dckey (dict-closure key)))
  (cond  ((null? (partdec key)) key)
         ((null? dckey) key)
         ((equal? #f dckey) #f)
       (else (dictionary-closure dckey)))))

(define (dict-closure key)
  (checking (words key) key))

(define ( partdec key) (filters (map (lambda (x) ( utils:decrypt key x)) utils:cipher-word-list)))

(define (filters l)
  (cond ((null? l) '())
        (else (if (null? (filter (lambda (x) (char-lower-case? x)) (string->list (car l)))) (filters (cdr l))
                  (append (list (car l)) (filters (cdr l)))))))

(define (subs cipw plaw)
  (subs1 (string->list cipw) (string->list plaw)))

(define (subs1 c p)
      (cond  ((null? c) '())
           ((equal? (car p) (car c)) (subs1 (cdr c) (cdr p)))
         (else (append (list (cons (car p) (car c))) (subs1 (cdr c) (cdr p))))))

   (define (words key)
  (map (lambda (x) (matcher x utils:dictionary key)) (partdec key)))
(define (checking l key)
  (cond ((null? l) '())
        (else (let* ((carl (car l))
                     (lencarl (length carl))
                     (caarl (car carl))
                     )
  (if (= 1 lencarl) #f
      (if (= 2 lencarl) (utils:add-substitution (subs (cadar l) caarl) key)
                                                          
          (checking (cdr l) key)))))))

(define (matcher x l key)
  (matcher1 x (filter (lambda (y) (equal? (string-length x) (string-length y))) l)
            (list x) key))

(define (matcher1 x l acc key)
  (cond ((= (length acc) 3) acc)
        ((null? l) acc)
        (else (if (match-strings x (car l) key ) (matcher1 x (cdr l) (cons (car l) acc) key)
                  (matcher1 x (cdr l) acc key)))))


(define (match-strings str1 str2 key)
  (same (string->list str1) (string->list str2) key)) 
(define (same l1 l2 key)
  (define key1 key)
  (define (same1 l1 l2 key1)
  (cond  ((null? l1) #t)
        ((equal? (car l1) (car l2)) (same1 (cdr l1) (cdr l2) key1))
        ((char-upper-case? (car l1)) #f)
        ((equal? (list-ref key (- (char->integer (car l2)) 65)) #\_)
                                      (same1 (cdr l1) (cdr l2) (utils:add-substitution (list (cons (car l2) (car l1))) key1)))
        (else #f)))
  (same1 l1 l2 key1))