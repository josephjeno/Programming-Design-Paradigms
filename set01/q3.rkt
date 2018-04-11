;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q3.rkt")
      
(provide kelvin-to-fahrenheit)

;; A Kelvin is represented as a Real, measured in degrees

;; A Fahrenheit is represented as a Real, measured in degrees

;; kelvin-to-fahrenheit : Kelvin -> Fahrenheit
;; GIVEN: temperature k in Kelvin
;; RETURNS: the temperature in Fahrenheit

;; EXAMPLES:
;; (kelvin-to-fahrenheit 1000) => 1340.33
;; (kelvin-to-fahrenheit 0)  => -459.67
;; (kelvin-to-fahrenheit -1000) => -2259.67

;; DESIGN STRATEGY: Transcribe Formula

(define (kelvin-to-fahrenheit k)
  (- (* k 9/5) 459.67))

;; TESTS
(begin-for-test
  (check-equal? (kelvin-to-fahrenheit 1000) 1340.33
                "(kelvin-to-fahrenheit 1000) did not equal 1340.33")
  (check-equal? (kelvin-to-fahrenheit 0) -459.67
                "(kelvin-to-fahrenheit 0) did not equal -459.67")
  (check-equal? (kelvin-to-fahrenheit -1000) -2259.67
                "(kelvin-to-fahrenheit -1000) did not equal -2259.67"))