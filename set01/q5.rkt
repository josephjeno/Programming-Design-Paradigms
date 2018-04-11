;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q5.rkt")
      
(provide years-to-test)

;; A YEAR-IN-DAYS is defined as a PosInt, measured in days
(define YEAR-IN-DAYS 365)
;; A DAY-IN-HOURS is defined as a PosInt, measured in hours
(define DAY-IN-HOURS 24)
;; a HOUR-IN-MINUTES is defined as a PosInt, measured in minutes
(define HOUR-IN-MINUTES 60)
;; a MINUTE-IN-SECONDS is defined as a PosInt, measured in seconds
(define MINUTE-IN-SECONDS 60)

;; a ProcessorSpeed is represented as a PosInt, measured in FLOPS (floating
;; point operations per second)

;; flopy : ProcessorSpeed -> PosInt
;; GIVEN: speed s of a microprocesssor in FLOPS
;; RETURNS: the number of floating point operations it can perform in one year

;; EXAMPLES:
;; (flopy 5) => 157680000
;; (flopy 1)  => 31536000

;; DESIGN STRATEGY: Transcribe Formula

(define (flopy s)
  (* YEAR-IN-DAYS
     DAY-IN-HOURS
     HOUR-IN-MINUTES
     MINUTE-IN-SECONDS
     s))

;; TESTS
(begin-for-test
  (check-equal? (flopy 5) 157680000
                "(flopy 5) did not equal 157680000")
  (check-equal? (flopy 1) 31536000
                "(flopy 1) did not equal 31536000"))

;; years-to-test : ProcessorSpeed -> PosReal
;; GIVEN: speed s of a microprocesssor in FLOPS
;; RETURNS: the number of 365-day years it would take to test the double
;;          precision addition operation on all legal inputs

;; EXAMPLES:
;; (years-to-test 1) =>
;; 2658455991569831745807614120560689152/246375
;; (years-to-test 5) =>
;; 2658455991569831745807614120560689152/1231875-

;; DESIGN STRATEGY: Transcribe Formula

(define (years-to-test s)
  (/ (expt 2 128) (flopy s)))

;; TESTS
(begin-for-test
  (check-equal? (years-to-test 1)
                2658455991569831745807614120560689152/246375
                "(years-to-test 1) did not equal expected answer")
  (check-equal? (years-to-test 5)
                2658455991569831745807614120560689152/1231875
                "(years-to-test 5) did not equal expected answer"))