;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q4.rkt")
      
(provide flopy)

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