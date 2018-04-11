;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(check-location "02" "q1.rkt")
      
(provide
 make-lexer
 lexer-token
 lexer-input
 initial-lexer
 lexer-stuck?
 lexer-shift
 lexer-reset)

;; REPRESENTATION:
;; a lexer is represented as a struct (make-lexer token input)
;; with the following fields:
;; token : String   is the token field
;; input : String   is the input field

;; IMPLEMENTATION
(define-struct lexer (token input))

;; CONSTRUCTOR TEMPLATE:
;; A Lexer is a
;;   (make-lexer String String)

;; OBSERVER TEMPLATE
;; lexer-fn : Lexer -> ??
;; (define (lexer-fn l)
;;   (...
;;    (lexer-token l)
;;    (lexer-input l)))

;;; Sample Lexers created for testing purposes
(define LEXER-STANDARD (make-lexer "abc" "1234"))
(define LEXER-SYMBOL (make-lexer "abc" "+1234"))
(define LEXER-EMPTY-INPUT (make-lexer "abc" ""))
(define LEXER-EMPTY-TOKEN (make-lexer "" "1234"))


;;; make-lexer : String String -> Lexer
;;; GIVEN: two strings s1 and s2
;;; RETURNS: a Lexer whose token string is s1
;;;     and whose input string is s2
;;; EXAMPLE:
;;; (make-lexer "abc" "1234") => LEXER-STANDARD
;;; (make-lexer "abc" "+1234") => LEXER-SYMBOL
;;; (make-lexer "abc" "") => LEXER-EMPTY-INPUT

;;; Defined during implementation of Lexer

;;; DESIGN STRATEGY
;;; Use constructor template for Lexer

;;; TESTS
(begin-for-test
  (check-equal? (make-lexer "abc" "1234") LEXER-STANDARD
                "make-lexer test 1 did not calculate correctly")
  (check-equal? (make-lexer "abc" "+1234") LEXER-SYMBOL
                "make-lexer test 2 did not calculate correctly")
  (check-equal? (make-lexer "abc" "") LEXER-EMPTY-INPUT
                "make-lexer test 3 did not calculate correctly"))


;;; lexer-token : Lexer -> String
;;; GIVEN: a Lexer
;;; RETURNS: its token string
;;; EXAMPLE:
;;;     (lexer-token (make-lexer "abc" "1234")) =>  "abc"

;;; Defined during implementation of Lexer

;;; DESIGN STRATEGY
;;; Use observer template for Lexer

;;; TESTS
(begin-for-test
  (check-equal? (lexer-token (make-lexer "abc" "1234")) "abc"
                "lexer-token test 1 did not calculate correctly"))

;;; lexer-input : Lexer -> String
;;; GIVEN: a Lexer
;;; RETURNS: its input string
;;; EXAMPLE:
;;;     (lexer-input (make-lexer "abc" "1234")) =>  "1234"

;;; Defined during implementation of Lexer

;;; DESIGN STRATEGY
;;; Use observer template for Lexer

;;; TESTS
(begin-for-test
  (check-equal? (lexer-input (make-lexer "abc" "1234")) "1234"
                "lexer-input test 1 did not calculate correctly"))


;;; initial-lexer : String -> Lexer
;;; GIVEN: an arbitrary string
;;; RETURNS: a Lexer lex whose token string is empty
;;;     and whose input string is the given string
;;; EXAMPLE:
;;;     (initial-lexer "1234") => LEXER-EMPTY-TOKEN
(define (initial-lexer s)
  (make-lexer "" s))

;;; DESIGN STRATEGY
;;; Use constructor template for Lexer

;;; TESTS
(begin-for-test
  (check-equal? (initial-lexer "1234") LEXER-EMPTY-TOKEN
                "initial-lexer test 1 did not calculate correctly"))


;;; string-numeric-or-alphabetic? : String -> Boolean
;;; GIVEN: a String
;;; RETURNS: true if the letters in the string are either all numeric or
;;; all alphabetic
;;; EXAMPLES:
;;; (string-numeric-or-alphabetic? "1234") => true
;;; (string-numeric-or-alphabetic? "abc") => true
;;; (string-numeric-or-alphabetic? "+1234") => false
(define (string-numeric-or-alphabetic? s)
  (or (string-numeric? s) (string-alphabetic? s)))

;;; DESIGN STRATEGY
;;; Combine simpler functions

;;; TESTS
(begin-for-test
  (check-equal? (string-numeric-or-alphabetic? "1234") true
                "string-num...alphabetic? test 1 did not calculate correctly")
  (check-equal? (string-numeric-or-alphabetic? "abc") true
                "string-num...alphabetic? test 2 did not calculate correctly")
  (check-equal? (string-numeric-or-alphabetic? "+1234") false
                "string-num...alphabetic? test 3 did not calculate correctly"))


;;; lexer-empty-input? : Lexer -> Boolean
;;; GIVEN: a Lexer
;;; RETURNS: true if the input string of the Lexer is empty, otherwise false
;;; EXAMPLES:
;;; (lexer-empty-input? LEXER-EMPTY-INPUT) =>  true
;;; (lexer-empty-input? LEXER-STANDARD) => false
(define (lexer-empty-input? l)
  (= (string-length (lexer-input l)) 0))

;;; DESIGN STRATEGY
;;; Transcribe formula

;;; TESTS
(begin-for-test
  (check-equal? (lexer-empty-input? LEXER-EMPTY-INPUT) true
                "lexer-empty-input? test 1 did not calculate correctly")
  (check-equal? (lexer-empty-input? LEXER-STANDARD) false
                "lexer-empty-input? test 2 did not calculate correctly"))


;;; lexer-stuck? : Lexer -> Boolean
;;; GIVEN: a Lexer
;;; RETURNS: false if and only if the given Lexer's input string
;;;     is non-empty and begins with an English letter or digit;
;;;     otherwise returns true.
;;; EXAMPLES:
;;;     (lexer-stuck? (make-lexer "abc" "1234"))  =>  false
;;;     (lexer-stuck? (make-lexer "abc" "+1234"))  =>  true
;;;     (lexer-stuck? (make-lexer "abc" ""))  =>  true
(define (lexer-stuck? l)
  (cond [(lexer-empty-input? l) true]
        [(string-numeric-or-alphabetic? (string-ith (lexer-input l) 0)) false]
        [else true]))

;;; DESIGN STRATEGY
;;; Cases on lexer-input

;;; TESTS
(begin-for-test
  (check-equal? (lexer-stuck? LEXER-STANDARD) false
                "(lexer-stuck? LEXER-STANDARD) did not calculate correctly")
  (check-equal? (lexer-stuck? LEXER-SYMBOL) true
                "(lexer-shift LEXER-SYMBOL) did not calculate correctly")
  (check-equal? (lexer-stuck? LEXER-EMPTY-INPUT) true
                "(lexer-stuck? LEXER-EMPTY-INPUT) did not calculate correctly"))


;;; lexer-shift : Lexer -> Lexer
;;; GIVEN: a Lexer
;;; RETURNS:
;;;   If the given Lexer is stuck, returns the given Lexer.
;;;   If the given Lexer is not stuck, then the token string
;;;       of the result consists of the characters of the given
;;;       Lexer's token string followed by the first character
;;;       of that Lexer's input string, and the input string
;;;       of the result consists of all but the first character
;;;       of the given Lexer's input string.
;;; EXAMPLES:
;;;     (lexer-shift (make-lexer "abc" ""))
;;;         =>  (make-lexer "abc" "")
;;;     (lexer-shift (make-lexer "abc" "+1234"))
;;;         =>  (make-lexer "abc" "+1234")
;;;     (lexer-shift (make-lexer "abc" "1234"))
;;;         =>  (make-lexer "abc1" "234")
(define (lexer-shift l)
  (cond [(lexer-stuck? l) l]
        [else (make-lexer
               (string-append (lexer-token l) (string-ith (lexer-input l) 0))
               (substring (lexer-input l) 1))]))

;;; DESIGN STRATEGY
;;; Combine simpler functions and use constructor template for Lexer

;;; TESTS
(begin-for-test
  (check-equal? (lexer-shift LEXER-EMPTY-INPUT) LEXER-EMPTY-INPUT
                "(lexer-shift LEXER-EMPTY-INPUT) did not calculate correctly")
  (check-equal? (lexer-shift LEXER-SYMBOL) LEXER-SYMBOL
                "(lexer-shift LEXER-SYMBOL) did not calculate correctly")
  (check-equal? (lexer-shift LEXER-STANDARD) (make-lexer "abc1" "234")
                "(lexer-shift LEXER-STANDARD) did not calculate correctly"))

;;; lexer-reset : Lexer -> Lexer
;;; GIVEN: a Lexer
;;; RETURNS: a Lexer whose token string is empty and whose
;;;     input string is empty if the given Lexer's input string
;;;     is empty and otherwise consists of all but the first
;;;     character of the given Lexer's input string.
;;; EXAMPLES:
;;;     (lexer-reset (make-lexer "abc" ""))
;;;         =>  (make-lexer "" "")
;;;     (lexer-reset (make-lexer "abc" "+1234"))
;;;         =>  (make-lexer "" "1234")
(define (lexer-reset l)
  (cond [(lexer-empty-input? l) (make-lexer "" "")]
        [else (make-lexer "" (substring (lexer-input l) 1))]))

;;; DESIGN STRATEGY
;;; Cases on lexer-input and use constructor template for Lexer

;;; TESTS
(begin-for-test
  (check-equal? (lexer-reset LEXER-EMPTY-INPUT) (make-lexer "" "")
                "(lexer-reset LEXER-EMPTY-INPUT) did not calculate correctly")
  (check-equal? (lexer-reset LEXER-SYMBOL) (make-lexer "" "1234")
                "(lexer-reset LEXER-SYMBOL) did not calculate correctly"))
