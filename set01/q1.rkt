;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q1.rkt")
      
(provide pyramid-volume)

;; pyramid-volume : PosReal PosReal -> PosReal
;; GIVEN: height h in meters and bottom side x in meters
;;        of a pyramid with a square base
;; RETURNS: the volume of the pyramid in cubic meters

;; EXAMPLES:
;; (pyramid-volume 5 12) => 100
;; (pyramid-volume 1 1)  => 1/3
;; (pyramid-volume .5 .12) => 0.01

;; DESIGN STRATEGY: Transcribe Formula

(define (pyramid-volume x h)
  (/ (* x x h) 3))

;; TESTS
(begin-for-test
  (check-equal? (pyramid-volume 5 12) 100
                "(pyramid-volume 5 12) did not equal 100")
  (check-equal? (pyramid-volume 1 1) 1/3
                "(pyramid-volume 1 1) did not equal 1/3")
  (check-equal? (pyramid-volume .5 .12) 0.01
                "(pyramid-volume .5 .12) did not equal 0.01"))