;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q2.rkt")
      
(provide furlongs-to-barleycorns)

;; An INCH-IN-BARLEYCORNS is defined as an PosInt, measured in barleycorns
(define INCH-IN-BARLEYCORNS 3)

;; A FOOT-IN-INCHES is defined as a PosInt, measured in inches
(define FOOT-IN-INCHES 12)

;; A ROD-IN-FEET is defined as a PosReal, measured in feet
(define ROD-IN-FEET 16.5)

;; A CHAIN-IN-RODS is defined as a PosInt, measured in rods
(define CHAIN-IN-RODS 4)

;; A FURLONG-IN-CHAINS is defined as a PosInt, measured in chains
(define FURLONG-IN-CHAINS 10)

;; furlongs-to-barleycorns : NonNegReal -> NonNegReal
;; GIVEN: length l in furlongs
;; RETURNS: the number of barleycorns in length l

;; EXAMPLES:
;; (furlongs-to-barleycorns 5) => 118800
;; (furlongs-to-barleycorns .5)  => 11880
;; (furlongs-to-barleycorns 0)  => 0

;; DESIGN STRATEGY: Transcribe Formula

(define (furlongs-to-barleycorns l)
  (* FURLONG-IN-CHAINS
     CHAIN-IN-RODS
     ROD-IN-FEET
     FOOT-IN-INCHES
     INCH-IN-BARLEYCORNS
     l))

;; TESTS
(begin-for-test
  (check-equal? (furlongs-to-barleycorns 5) 118800
                "(furlongs-to-barleycorns 5) did not equal 118800")
  (check-equal? (furlongs-to-barleycorns .5) 11880
                "(furlongs-to-barleycorns .5) did not equal 11880")
  (check-equal? (furlongs-to-barleycorns 0) 0
                "(furlongs-to-barleycorns 0) did not equal 0"))