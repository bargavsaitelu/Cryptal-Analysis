#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
 ; key)

(define (secret-word-enumeration key-after-dictionary-closure)
  ;; Returns a key or false (#f)
  (define (first n l)
  (if (= n 0) '()
      (append (list (car l)) (first (- n 1) (cdr l)))))
  (let* ((keydc key-after-dictionary-closure)
         (f6 (first 6 keydc))
         (possiblesw (matchedwords (string-upcase (list->string f6)) utils:dictionary)))
    (define (wordtokey l)
  (if (null? l) '()
      (if (checker1 (list->string keydc) (list->string (utils:encryption-key (car l)))) (append (list (utils:encryption-key (car l)) )
                                                                  (wordtokey (cdr l)))
          (wordtokey (cdr l)))))
    (cond ((null? possiblesw) #f)
          ((= 1 (length possiblesw)) (if (checker1 (list->string keydc) (list->string (utils:encryption-key (car possiblesw))))
                                         (utils:encryption-key (car possiblesw))
                                         #f))
          (else (cond ((null? (wordtokey possiblesw)) #f)
                      ((= 1 (length (wordtokey possiblesw))) (car (wordtokey possiblesw)))
                      (else keydc))))))


(define (checker1 str1 str2)
  (checker (string->list str1) (string->list str2)))
(define (checker s1 s2 )
  (cond  ((not (= (length s1) (length s2))) #f)
         ((null? s2) #t)
        ((equal? (car s1) #\_) (checker (cdr s1) (cdr s2) ))
        ((equal? (car s1) (car s2)) (checker (cdr s1) (cdr s2) ))
        (else #f)))

(define (matchedwords sw l)
  (if (null? l) '()
  (if (checker1 sw (car l)) (append (list (car l)) (matchedwords sw (cdr l)))
      (matchedwords sw (cdr l)))))


