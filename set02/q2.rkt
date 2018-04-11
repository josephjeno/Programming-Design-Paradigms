;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(check-location "02" "q2.rkt")

(provide
 initial-state
 next-state
 is-red?
 is-green?)

;;; REPRESENTATION
;;; a ChineseTrafficSignal is represented as a struct
;;;    (make-chinese-traffic-signal initial current state)
;;; with the following fields:
;;; initial : PosInt      is the time at the start of its red state, in seconds
;;; current : NonZeroInt  is the time remaining in its current state, in seconds
;;; state   : String      is the current state (red, green, or blank) associated
;;;                       with the current time

;;; IMPLEMENTATION
(define-struct chinese-traffic-signal (initial current state))

;;; CONSTRUCTOR TEMPLATE
;;; (make-chinese-traffic-signal PosInt NonZeroInt String)

;;; OBSERVER TEMPLATE
;;; chinese-traffic-signal-fn : ChineseTrafficSignal -> ??
;;; (define (chinese-traffic-signal-fn s)
;;;   (... (chinese-traffic-signal-initial s)
;;;        (chinese-traffic-signal-current s)
;;;        (chinese-traffic-signal-state s)))


;;; signal-state : NonZeroInt -> String
;;; GIVEN: a NonZeroInt representing the time of a ChineseTrafficSignal, in secs
;;; RETURNS: the state that the traffic signal is in (red, green, or blank)
;;; EXAMPLE:
;;; (signal-state (initial-state 4)) => "red"
;;; (signal-state (next-state
;;;                (next-state
;;;                 (next-state
;;;                  (next-state (initial-state 4))))))  =>  "green"
;;; (signal-state (next-state
;;;                (next-state
;;;                 (next-state
;;;                  (next-state
;;;                   (next-state (initial-state 4)))))))  =>  "blank"
(define (signal-state current)
  (cond [(> current 0) "red"]
        [(< current -3) "green"]
        [(= current -3) "blank"]
        [(= current -2) "green"]
        [(= current -1) "blank"]))

;;; DESIGN STRATEGY
;;; Cases on current

;; TESTS
(begin-for-test
  (check-equal? (signal-state 30) "red"
                "(signal-state 30) did not equal red")
  (check-equal? (signal-state -30) "green"
                "(signal-state -30) did not equal green")
  (check-equal? (signal-state -3) "blank"
                "(signal-state -3) did not equal blank")
  (check-equal? (signal-state -2) "green"
                "(signal-state -2) did not equal green")
  (check-equal? (signal-state -1) "blank"
                "(signal-state -1) did not equal blank"))


;;; signal-increment : PosInt NonZeroInt -> NonZeroInt
;;; GIVEN: a PosInt representing the time of the initial red state in secs
;;;        and a NonZeroInt representing the time of the current state in secs
;;; RETURNS: the integer value one second later
;;; EXAMPLES:
;;; (signal-increment 4 4)  => 3
;;; (signal-increment 4 1)  => -4
;;; (signal-increment 4 -4) => -3
;;; (signal-increment 4 -1) => 4
(define (signal-increment initial current)
  (cond [(= current 1) (* -1 initial)]
        [(= current -1) initial]
        [(> current 1) (- current 1)]
        [(< current -1) (+ current 1)]))

;;; DESIGN STRATEGY
;;; Cases on current

;;; TESTS
(begin-for-test
  (check-equal? (signal-increment 4 4) 3
                "signal-increment test 1 did not equal 3")
  (check-equal? (signal-increment 4 1) -4
                "signal-increment test 2 did not equal -4")
  (check-equal? (signal-increment 4 -4) -3
                "signal-increment test 3 did not equal -3")
  (check-equal? (signal-increment 4 -1) 4
                "signal-increment test 4 did not equal 4"))


;;; initial-state : PosInt -> ChineseTrafficSignal
;;; GIVEN: an integer n greater than 3
;;; RETURNS: a representation of a Chinese traffic signal
;;;     at the beginning of its red state, which will last
;;;     for n seconds
;;; EXAMPLE:
;;;     (is-red? (initial-state 4))  =>  true
(define (initial-state n)
  (make-chinese-traffic-signal n n "red"))

;;; DESIGN STRATEGY
;;; Use constructor template for ChineseTrafficSignal

;;; TESTS
(begin-for-test
  (check-equal? (initial-state 4) (make-chinese-traffic-signal 4 4 "red")
                "(initial-state 4) did not make expected traffic signal")
  (check-equal? (is-red? (initial-state 4)) true
                "(is-red? (initial-state 4)) did not equal true"))


;;; next-state : ChineseTrafficSignal -> ChineseTrafficSignal
;;; GIVEN: a representation of a traffic signal in some state
;;; RETURNS: the state that traffic signal should have one
;;;     second later
;;; EXAMPLES:
;;;     (next-state (initial-state 4)) =>
;;;          (make-chinese-traffic-signal 4 4 "red")
(define (next-state s)
  (make-chinese-traffic-signal
   (chinese-traffic-signal-initial s)
   (signal-increment (chinese-traffic-signal-initial s)
                     (chinese-traffic-signal-current s))
   (signal-state (signal-increment (chinese-traffic-signal-initial s)
                                   (chinese-traffic-signal-current s)))))

;;; DESIGN STRATEGY
;;; Use constructor template for ChineseTrafficSignal

;;; TESTS
(begin-for-test
  (check-equal? (next-state (initial-state 4))
                (make-chinese-traffic-signal 4 3 "red")
                "next-state test 1 did not make expected traffic signal")
  (check-equal? (next-state (make-chinese-traffic-signal 4 1 "red"))
                (make-chinese-traffic-signal 4 -4 "green")
                "next-state test 2 did not make expected traffic signal")
  (check-equal? (next-state (make-chinese-traffic-signal 4 -4 "green"))
                (make-chinese-traffic-signal 4 -3 "blank")
                "next-state test 3 did not make expected traffic signal"))


;;; is-red? : ChineseTrafficSignal -> Boolean
;;; GIVEN: a representation of a traffic signal in some state
;;; RETURNS: true if and only if the signal is red
;;; EXAMPLES:
;;;     (is-red? (next-state (initial-state 4)))  =>  true
;;;     (is-red?
;;;      (next-state
;;;       (next-state
;;;        (next-state (initial-state 4)))))  =>  true
;;;     (is-red?
;;;      (next-state
;;;       (next-state
;;;        (next-state
;;;         (next-state (initial-state 4))))))  =>  false
;;;     (is-red?
;;;      (next-state
;;;       (next-state
;;;        (next-state
;;;         (next-state
;;;          (next-state (initial-state 4)))))))  =>  false
(define (is-red? s)
  (string=? (chinese-traffic-signal-state s) "red"))

;;; DESIGN STRATEGY
;;; Combine simpler functions

;; TESTS
(begin-for-test
  (check-equal? (is-red? (next-state (initial-state 4))) true
                "is-red? test 1 did not equal true")
  (check-equal? (is-red? (next-state
                          (next-state
                           (next-state (initial-state 4))))) true
                "is-red? test 2 did not equal true")
  (check-equal? (is-red? (next-state
                          (next-state
                           (next-state
                            (next-state (initial-state 4)))))) false
                "is-red? test 3 did not equal false")
  (check-equal? (is-red? (next-state
                          (next-state
                           (next-state
                            (next-state
                             (next-state (initial-state 4))))))) false
                "is-red? test 4 did not equal false"))


;;; is-green? : ChineseTrafficSignal -> Boolean
;;; GIVEN: a representation of a traffic signal in some state
;;; RETURNS: true if and only if the signal is green
;;; EXAMPLES:
;;;     (is-green?
;;;      (next-state
;;;       (next-state
;;;        (next-state
;;;         (next-state (initial-state 4))))))  =>  true
;;;     (is-green?
;;;      (next-state
;;;       (next-state
;;;        (next-state
;;;         (next-state
;;;          (next-state (initial-state 4)))))))  =>  false
(define (is-green? s)
  (string=? (chinese-traffic-signal-state s) "green"))

;;; DESIGN STRATEGY
;;; Combine simpler functions

;; TESTS
(begin-for-test
  (check-equal? (is-green? (next-state
                            (next-state
                             (next-state
                              (next-state (initial-state 4)))))) true
                "is-green? test 1 did not equal true")
  (check-equal? (is-green? (next-state
                            (next-state
                             (next-state
                              (next-state
                               (next-state (initial-state 4))))))) false
                "is-green? test 2 did not equal false"))

