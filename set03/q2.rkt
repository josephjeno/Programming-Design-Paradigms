;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

(check-location "03" "q2.rkt")

(provide simulation
         initial-world
         world-ready-to-serve?
         world-after-tick
         world-after-key-event
         world-ball
         world-racket
         ball-x
         ball-y
         racket-x
         racket-y
         ball-vx
         ball-vy
         racket-vx
         racket-vy
         world-after-mouse-event
         racket-after-mouse-event
         racket-selected?)


;;; start with (simulation PosReal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;;; dimensions of the court, in pixels
(define COURT-WIDTH 425)
(define COURT-HEIGHT 649)
(define COURT-WALLS-COLOR "black")
(define COURT-FLOOR-COLOR "white")

;;; specifications of the ball, in pixels
(define BALL-RADIUS 3)
(define BALL-OUTLINE "solid")
(define BALL-COLOR "black")

;;; specifications of the racket, in pixels
(define RACKET-WIDTH 47)
(define RACKET-HEIGHT 7)
(define RACKET-HALF (quotient RACKET-WIDTH 2))
(define RACKET-OUTLINE "solid")
(define RACKET-COLOR "green")

;;; specfications of the mouse cursor, in pixels
(define MOUSE-RADIUS 4)
(define MOUSE-OUTLINE "solid")
(define MOUSE-COLOR "blue")

;;; specifications of ready-to-serve state
(define RTS-X 330)
(define RTS-Y 384)
(define RTS-VX 0)
(define RTS-VY 0)

;;; specifications of rally state
(define RALLY-VX 3)
(define RALLY-VY -9)
(define RALLY-PAUSE-lENGTH 3)
(define RALLY-COURT-FLOOR-COLOR "yellow")

;;; an empty court image
(define EMPTY-COURT (empty-scene COURT-WIDTH
                                 COURT-HEIGHT))

;;; a paused court image
(define PAUSED-COURT (empty-scene COURT-WIDTH
                                  COURT-HEIGHT
                                  RALLY-COURT-FLOOR-COLOR))

;;; a ball image
(define BALL (circle BALL-RADIUS
                     BALL-OUTLINE
                     BALL-COLOR))

;;; a racket image
(define RACKET (rectangle RACKET-WIDTH
                          RACKET-HEIGHT
                          RACKET-OUTLINE
                          RACKET-COLOR))

;;; a mouse image
(define MOUSE (circle MOUSE-RADIUS
                      MOUSE-OUTLINE
                      MOUSE-COLOR))

;;; END CONSTANTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;;; A Ball is represented as a
;;;    (make-ball x y vx vy)
;;; with the following fields:
;;;  x,  y : Integer    the x or y coordinate of the ball's position,
;;;                     in graphics coordinates
;;; vx, vy : Integer    the vx or vy component of the ball's velocity,
;;;                     in pixels per tick

;;; IMPLEMENTATION
(define-struct ball (x y vx vy))

;;; CONSTRUCTOR TEMPLATE
;;; A Ball is a (make-ball Integer Integer Integer Integer)

;;; OBSERVER TEMPLATE
;;; ball-fn : Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))


;;; A Racket is represented as a
;;;   (make-racket x y vx vy selected? mx my)
;;; with the following fields:
;;;     x,  y : Integer    the x or y coordinate of the racket's position,
;;;                        in graphics coordinates
;;;    vx, vy : Integer    the vx or vy component of the racket's velocity,
;;;                        in pixels per tick
;;; selected? : Boolean    true if the racket is selected by the mouse
;;;   mx, my  : Integer    the x or y coordinate difference between the mouses's
;;;                        position, relative to the racket.
;;;                        (0,0 if not selected)

;;; IMPLEMENTATION
(define-struct racket (x y vx vy selected? mx my))

;;; CONSTRUCTOR TEMPLATE
;;; A Racket is a (make-racket Integer Integer Integer
;;;                    Integer Boolean Integer Integer)

;;; OBSERVER TEMPLATE
;;; racket-fn : Racket -> ??
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       (racket-selected? r)
       (racket-mx r)
       (racket-my r)))

;;; used for testing
(define B-TEST (make-ball 10 20 30 40))
(define R-TEST (make-racket 50 60 70 80 false 0 0))

(define B-COLLIDE-R (make-ball 350 360 0 10))
(define R-COLLIDE-B (make-racket 350 365 5 0 false 0 0))

(define B-MISS-R (make-ball 350 360 0 10))
(define R-MISS-B (make-racket 350 365 0 10 false 0 0))

(define B-COLLIDE-RIGHT (make-ball 420 360 10 0))
(define R-COLLIDE-RIGHT (make-racket 400 360 10 0 false 0 0))

(define B-COLLIDE-LEFT (make-ball 5 360 -10 0))
(define R-COLLIDE-LEFT (make-racket 30 360 -10 0 false 0 0))

(define B-COLLIDE-TOP (make-ball 350 5 0 -10))
(define R-COLLIDE-TOP (make-racket 350 5 0 -10 false 0 0))

(define B-COLLIDE-BOTTOM (make-ball 350 645 0 10))
(define R-COLLIDE-BOTTOM (make-racket 350 645 0 10 false 0 0))

(define B-RTS (make-ball RTS-X RTS-Y RTS-VX RTS-VY))
(define R-RTS (make-racket RTS-X RTS-Y RTS-VX RTS-VY false 0 0))


;;; ball-x : Ball -> Integer
;;; ball-y : Ball -> Integer
;;; racket-x : Racket -> Integer
;;; racket-y : Racket -> Integer
;;; GIVEN: a racket or ball
;;; RETURNS: the x or y coordinate of that item's position,
;;;     in graphics coordinates
;;; EXAMPLES:
;;; (ball-x B-TEST) -> 10
;;; (ball-y B-TEST) -> 20
;;; (racket-x R-TEST) -> 50
;;; (racket-y R-TEST) -> 60
;;; DESIGN STRATEGY:
;;; Use observer template

;;; Defined during implementation of that item

;;; TESTS:
(begin-for-test
  (check-equal? (ball-x B-TEST) 10
                "ball-x test 1 did not calculate correctly")
  (check-equal? (ball-y B-TEST) 20
                "ball-y test 1 did not calculate correctly")
  (check-equal? (racket-x R-TEST) 50
                "racket-x test 1 did not calculate correctly")
  (check-equal? (racket-y R-TEST) 60
                "racket-y test 1 did not calculate correctly"))

          
;;; ball-vx : Ball -> Integer
;;; ball-vy : Ball -> Integer
;;; racket-vx : Racket -> Integer
;;; racket-vy : Racket -> Integer
;;; GIVEN: a racket or ball
;;; RETURNS: the vx or vy component of that item's velocity,
;;;     in pixels per tick
;;; EXAMPLES:
;;; (ball-vx (make-ball 10 20 30 40)) -> 30
;;; (ball-vy (make-ball 10 20 30 40)) -> 40
;;; (racket-vx (make-racket 10 20 30 40 false 0 0)) -> 30
;;; (racket-vy (make-racket 10 20 30 40 false 0 0)) -> 40
;;; DESIGN STRATEGY:
;;; Use observer template

;;; Defined during implementation of that item

;;; TESTS:
(begin-for-test
  (check-equal? (ball-vx B-TEST) 30
                "ball-vx test 1 did not calculate correctly")
  (check-equal? (ball-vy B-TEST) 40
                "ball-vy test 1 did not calculate correctly")
  (check-equal? (racket-vx R-TEST) 70
                "racket-vx test 1 did not calculate correctly")
  (check-equal? (racket-vy R-TEST) 80
                "racket-vy test 1 did not calculate correctly"))

;;; racket-selected? : Racket-> Boolean
;;; GIVEN: a racket
;;; RETURNS: true iff the racket is selected

;;; DESIGN STRATEGY:
;;; Use observer template

;;; Defined during implementation of that item


;;; A World is represented as a
;;;    (make-world ball racket speed ready-to-serve? paused? plength)
;;; with the following fields:
;;;            ball : Ball     any valid ball
;;;          racket : Racket   any valid racket
;;;           speed : PosReal  the speed of the simulation in seconds per tick
;;; ready-to-serve? : Boolean  true if world in a ready-to-serve state
;;;         paused? : Boolean  true if world in a paused state
;;;         plength : PosReal  the time that has elapsed since pausing in
;;;                            seconds per tick

;;; IMPLEMENTATION
(define-struct world (ball racket speed ready-to-serve? paused? plength))

;;; CONSTRUCTOR TEMPLATE
;;; A World is a (make-world Ball Racket PosReal Boolean Boolean PosReal)

;;; OBSERVER TEMPLATE
;;; world-fn : World -> ??
(define (world-fn w)
  (... (world-ball w)
       (world-racket w)
       (world-speed w)
       (world-ready-to-serve? w)
       (world-paused? w)
       (world-plength w)))

;;; used for testing
(define W-TEST (make-world B-TEST R-TEST 1 true true 0))
(define W-PAUSED (make-world B-TEST R-TEST 1 false true 0))
(define W-RTS (make-world B-RTS R-RTS 1 true false 0))
(define W-NO-COLL (make-world B-MISS-R R-MISS-B 1 false false 0))
(define W-B-COLL-BOTTOM (make-world B-COLLIDE-BOTTOM R-TEST 1 false false 0))
(define W-R-COLL-BOTTOM (make-world B-TEST R-COLLIDE-BOTTOM 1 false false 0))
(define W-R-COLL-TOP (make-world B-TEST R-COLLIDE-TOP 1 false false 0))


;;; world-ball : World -> Ball
;;; GIVEN: a world
;;; RETURNS: the ball that's present in the world
;;; EXAMPLES
;;; (world-ball W-TEST) -> B-TEST
;;; DESIGN STRATEGY
;;; Use observer template

;;; Defined during implementation of world

;;; TESTS:
(begin-for-test
  (check-equal? (world-ball W-TEST) B-TEST
                "world-ball test 1 did not calculate correctly"))


;;; world-racket : World -> Racket
;;; GIVEN: a world
;;; RETURNS: the racket that's present in the world
;;; EXAMPLES
;;; (world-ball W-TEST) -> R-TEST
;;; DESIGN STRATEGY
;;; Use observer template

;;; Defined during implementation of world

;;; TESTS:
(begin-for-test
  (check-equal? (world-racket W-TEST) R-TEST
                "world-racket test 1 did not calculate correctly"))


;;; world-ready-to-serve? : World -> Boolean
;;; GIVEN: a world
;;; RETURNS: true iff the world is in its ready-to-serve state
;;; EXAMPLES
;;; (world-ball W-TEST) -> R-TEST
;;; DESIGN STRATEGY
;;; Use observer template

;;; Defined during implementation of world

;;; TESTS:
(begin-for-test
  (check-equal? (world-ready-to-serve? W-TEST) true
                "world-ready-to-serve? test 1 did not calculate correctly"))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CORE SIMULATION FUNCTIONS

;;; simulation : PosReal -> World
;;; GIVEN: the speed of the simulation, in seconds per tick
;;;     (so larger numbers run slower)
;;; EFFECT: runs the simulation, starting with the initial world
;;; RETURNS: the final state of the world
;;; EXAMPLES:
;;;     (simulation 1) runs in super slow motion
;;;     (simulation 1/24) runs at a more realistic speed
;;; DESIGN STRATEGY: Combine simpler functions following function requirements
;;;                  for big-bang.

(define (simulation speed)
  (big-bang (initial-world speed)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))


;;; world-to-scene : World -> Scene
;;; GIVEN: a World
;;; RETURNS: a Scene that portrays the given world
;;; EXAMPLE:
;;; (world-to-scene W-TEST should return a white canvas with a ball placed at
;;; (10,20) and a racket placed at (50,60)
;;; (world-to-scene W-RTS should return a yellow canvas with a ball placed at
;;; (330,384) and a racket placed at (330,384)
;;; DESIGN STRATEGY: Place items in world.

(define (world-to-scene w)
  (if (racket-selected? (world-racket w))
      (place-image BALL (ball-x (world-ball w)) (ball-y (world-ball w))
                   (place-image MOUSE (- (racket-x (world-racket w))
                                         (racket-mx (world-racket w)))
                                (- (racket-y (world-racket w))
                                   (racket-my (world-racket w)))
                                (place-image RACKET (racket-x (world-racket w))
                                             (racket-y (world-racket w))
                                             (if (world-paused? w) PAUSED-COURT EMPTY-COURT))))
      (place-image BALL (ball-x (world-ball w)) (ball-y (world-ball w))
                   (place-image RACKET (racket-x (world-racket w))
                                (racket-y (world-racket w))
                                (if (world-paused? w) PAUSED-COURT EMPTY-COURT)))))

;;; TESTS:
(begin-for-test
  (check-equal? (world-to-scene W-TEST)
                (place-image BALL 10 20
                             (place-image RACKET 50 60 PAUSED-COURT))
                "world-to-scene test 1 did not calculate correctly")
  (check-equal? (world-to-scene W-RTS)
                (place-image BALL 330 384
                             (place-image RACKET 330 384 EMPTY-COURT))
                "world-to-scene test 2 did not calculate correctly"))

;;; initial-world : PosReal -> World
;;; GIVEN: the speed of the simulation, in seconds per tick
;;;     (so larger numbers run slower)
;;; RETURNS: the ready-to-serve state of the world
;;; EXAMPLE:
;;; (initial-world 1) -> (make-world B-RTS R-RTS 1 true false 0)
;;; DESIGN STRATEGY: Follow constructor template for World

(define (initial-world speed)
  (make-world (make-ball RTS-X RTS-Y RTS-VX RTS-VY)
              (make-racket RTS-X RTS-Y RTS-VX RTS-VY false 0 0)
              speed
              true
              false
              0))

;;; TESTS:
(begin-for-test
  (check-equal? (initial-world 1) (make-world B-RTS R-RTS 1 true false 0)
                "initial-world test 1 did not calculate correctly"))

;;; world-after-tick : World -> World
;;; GIVEN: any world that's possible for the simulation
;;; RETURNS: the world that should follow the given world
;;;     after a tick
;;; EXAMPLE:
;;; (world-after-tick W-RTS) -> W-RTS
;;; (world-after-tick W-PAUSED) -> (world-reset W-PAUSED)
;;; (world-after-tick W-NO-COLL) -> (next-world W-NO-COLL)
;;; (world-after-tick W-B-COLL-BOTTOM) -> (world-reset W-B-COLL-BOTTOM)
;;; (world-after-tick W-R-COLL-BOTTOM) -> (world-reset W-R-COLL-BOTTOM)
;;; (world-after-tick W-R-COLL-TOP) -> (world-reset W-R-COLL-TOP)
;;; DESIGN STRATEGY: Cases on world states and reset collisions 

(define (world-after-tick w)
  (cond [(world-ready-to-serve? w) w]
        [(world-paused? w) (world-reset w)]
        [(racket-collide-front? (world-racket w)) (world-reset w)]
        [(racket-collide-back? (world-racket w)) (world-reset w)]
        [(ball-collide-back? (world-ball w)) (world-reset w)]
        [else (next-world w)]))

;;; TESTS:
(begin-for-test
  (check-equal? (world-after-tick W-RTS) W-RTS
                "world-after-tick test 1 did not calculate correctly")
  (check-equal? (world-after-tick W-PAUSED) (world-reset W-PAUSED)
                "world-after-tick test 2 did not calculate correctly")
  (check-equal? (world-after-tick W-NO-COLL) (next-world W-NO-COLL)
                "world-after-tick test 3 did not calculate correctly")
  (check-equal? (world-after-tick W-B-COLL-BOTTOM) (world-reset W-B-COLL-BOTTOM)
                "world-after-tick test 4 did not calculate correctly")
  (check-equal? (world-after-tick W-R-COLL-BOTTOM) (world-reset W-R-COLL-BOTTOM)
                "world-after-tick test 5 did not calculate correctly")
  (check-equal? (world-after-tick W-R-COLL-TOP) (world-reset W-R-COLL-TOP)
                "world-after-tick test 6 did not calculate correctly"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PAUSED WORLD

;;; world-reset : World -> World
;;; GIVEN: A world that is paused
;;; RETURNS: A world in a continued paused state or a fresh world
;;; EXAMPLE:
;;; (world-reset (make-world B-TEST R-TEST 1 false true 3)) => (initial-world 1)
;;; DESIGN STRATEGY: Cases on plength

(define (world-reset w)
  (if (< (world-plength w) RALLY-PAUSE-lENGTH )
      (make-world (paused-ball w)
                  (paused-racket w)
                  (world-speed w)
                  (world-ready-to-serve? w)
                  true
                  (+ (world-speed w) (world-plength w)))
      (initial-world (world-speed w))))

;;; TESTS:
(begin-for-test
  (check-equal? (world-reset (make-world B-TEST R-TEST 1 false true 3))
                (initial-world 1)
                "world-reset test 1 did not calculate correctly"))

;;; paused-ball : World -> Ball
;;; GIVEN: A paused world
;;; RETURNS: The ball in that world with no velocity
;;; EXAMPLE:
;;; (paused-ball W-TEST) => (make-ball 10 20 0 0)
;;; DESIGN STRATEGY: Constructor template for Ball

(define (paused-ball w)
  (make-ball (ball-x (world-ball w))
             (ball-y (world-ball w))
             0
             0))

;;; TESTS:
(begin-for-test
  (check-equal? (paused-ball W-TEST) (make-ball 10 20 0 0)
                "paused-ball test 1 did not calculate correctly"))

;;; paused-racket : World -> Ball
;;; GIVEN: A paused world
;;; RETURNS: The racket in that world with no velocity
;;; EXAMPLE:
;;; (paused-racket W-TEST) => (make-racket 50 60 0 0 false 0 0)
;;; DESIGN STRATEGY: Constructor template for Racket

(define (paused-racket w)
  (make-racket (racket-x (world-racket w))
               (racket-y (world-racket w))
               0
               0
               false
               0
               0))

;;; TESTS:
(begin-for-test
  (check-equal? (paused-racket W-TEST) (make-racket 50 60 0 0 false 0 0)
                "paused-racket test 1 did not calculate correctly"))

;;; END PAUSED WORLD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; next-world : World -> World
;;; GIVEN: a world in a rally state
;;; RETURNS: the world after a tick
;;; EXAMPLE: (next-world W-NO-COLL) => (make-world (make-ball 350 370 0 10)
;;;                                             (make-racket 350 375 0 10 f 0 0)
;;;                                                1
;;;                                                false
;;;                                                false
;;;                                                0)
;;; DESIGN STRATEGY: Constructor template for World

(define (next-world w)
  (make-world (new-ball w)
              (new-racket w)
              (world-speed w)
              (world-ready-to-serve? w)
              (world-paused? w)
              (world-plength w)))

;;; TESTS:
(begin-for-test
  (check-equal? (next-world W-NO-COLL)
                (make-world (make-ball 350 370 0 10)
                            (make-racket 350 375 0 10 false 0 0)
                            1
                            false
                            false
                            0)
                "next-world test 1 did not calculate correctly"))


;;; new-ball : World -> Ball
;;; GIVEN: a world in a rally state
;;; RETURNS: the ball in that world after 1 tick
;;; EXAMPLE:
;;; (new-ball W-NO-COLL) => (make-ball 350 370 0 10)
;;; DESIGN STRATEGY: Constructor template for Ball

(define (new-ball w)
  (make-ball (new-ball-x (world-ball w) (world-racket w))
             (new-ball-y (world-ball w) (world-racket w))
             (new-ball-vx (world-ball w) (world-racket w))
             (new-ball-vy (world-ball w) (world-racket w))))

;;; TESTS
(begin-for-test
  (check-equal? (new-ball W-NO-COLL) (make-ball 350 370 0 10)
                "new-ball test 1 did not calculate correctly"))


;;; new-ball-x : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the x-coordinate of the ball after 1 tick
;;; EXAMPLES:
;;; (new-ball-x B-COLLIDE-LEFT R-RTS) => 5
;;; (new-ball-x B-COLLIDE-RIGHT R-RTS) => 420
;;; (new-ball-x B-RTS R-RTS) => 330
;;; DESIGN STRATEGY: Cases on ball collisions, then transcribe formula

(define (new-ball-x b r)
  (cond [(ball-collide-left-side? b) (- (+ (ball-x b)
                                           (ball-vx b)))]
        [(ball-collide-right-side? b) (- COURT-WIDTH (- (+ (ball-x b)
                                                           (ball-vx b))
                                                        COURT-WIDTH))]
        [else (+ (ball-x b) (ball-vx b))]))

;;; TESTS
(begin-for-test
  (check-equal? (new-ball-x B-COLLIDE-LEFT R-RTS) 5
                "new-ball-x test 1 did not calculate correctly")
  (check-equal? (new-ball-x B-COLLIDE-RIGHT R-RTS) 420
                "new-ball-x test 2 did not calculate correctly")
  (check-equal? (new-ball-x B-RTS R-RTS) 330
                "new-ball-x test 3 did not calculate correctly"))


;;; new-ball-y : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the y-coordinate of the ball after 1 tick
;;; EXAMPLES:
;;; (new-ball-y B-COLLIDE-R R-COLLIDE-B) => 350
;;; (new-ball-y B-COLLIDE-TOP R-RTS) => 5
;;; (new-ball-y B-RTS R-RTS) => 384
;;; DESIGN STRATEGY: Cases on ball collisions, then transcribe formula

(define (new-ball-y b r)
  (cond [(ball-collide-racket? b r) (+ (ball-y b) (new-ball-vy b r))]
        [(ball-collide-front? b) (- (+ (ball-y b) (ball-vy b)))]
        [else (+ (ball-y b) (ball-vy b))]))

;;; TESTS
(begin-for-test
  (check-equal? (new-ball-y B-COLLIDE-R R-COLLIDE-B) 350
                "new-ball-y test 1 did not calculate correctly")
  (check-equal? (new-ball-y B-COLLIDE-TOP R-RTS) 5
                "new-ball-y test 2 did not calculate correctly")
  (check-equal? (new-ball-y B-RTS R-RTS) 384
                "new-ball-y test 3 did not calculate correctly"))


;;; new-ball-vx : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the x velocity of the ball after 1 tick
;;; EXAMPLES:
;;; (new-ball-vx B-COLLIDE-LEFT R-RTS) => 10
;;; (new-ball-vx B-COLLIDE-RIGHT R-RTS) => -10
;;; (new-ball-vx B-RTS R-RTS) => 0
;;; DESIGN STRATEGY: Cases on ball collisions, then transcribe formula

(define (new-ball-vx b r)
  (cond [(ball-collide-left-side? b) (- (ball-vx b))]
        [(ball-collide-right-side? b) (- (ball-vx b))]
        [else (ball-vx b)]))

;;; TESTS
(begin-for-test
  (check-equal? (new-ball-vx B-COLLIDE-LEFT R-RTS) 10
                "new-ball-vx test 1 did not calculate correctly")
  (check-equal? (new-ball-vx B-COLLIDE-RIGHT R-RTS) -10
                "new-ball-vx test 2 did not calculate correctly")
  (check-equal? (new-ball-vx B-RTS R-RTS) 0
                "new-ball-vx test 3 did not calculate correctly"))


;;; new-ball-vy : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the y-coordinate of the ball after 1 tick
;;; EXAMPLES:
;;; (new-ball-vy B-COLLIDE-R R-COLLIDE-B) => -10
;;; (new-ball-vy B-COLLIDE-TOP R-RTS) => 10
;;; (new-ball-vy B-RTS R-RTS) => 0
;;; DESIGN STRATEGY: Cases on ball collisions, then transcribe formula

(define (new-ball-vy b r)
  (cond [(ball-collide-racket? b r) (- (racket-vy r) (ball-vy b))]
        [(ball-collide-front? b) (- (ball-vy b))]
        [else (ball-vy b)]))

;;; TESTS
(begin-for-test
  (check-equal? (new-ball-vy B-COLLIDE-R R-COLLIDE-B) -10
                "new-ball-vy test 1 did not calculate correctly")
  (check-equal? (new-ball-vy B-COLLIDE-TOP R-RTS) 10
                "new-ball-vy test 2 did not calculate correctly")
  (check-equal? (new-ball-vy B-RTS R-RTS) 0
                "new-ball-vy test 3 did not calculate correctly"))


;;; new-racket : World -> Racket
;;; GIVEN: a world in a rally state
;;; RETURNS: the racket in that world after 1 tick
;;; EXAMPLE:
;;; (new-racket W-NO-COLL) => (make-racket 350 375 0 10 false)
;;; DESIGN STRATEGY: Constructor template for Racket

(define (new-racket w)
  (make-racket (new-racket-x (world-ball w) (world-racket w))
               (new-racket-y (world-ball w) (world-racket w))
               (new-racket-vx (world-ball w) (world-racket w))
               (new-racket-vy (world-ball w) (world-racket w))
               (racket-selected? (world-racket w))
               (racket-mx (world-racket w))
               (racket-my (world-racket w))))

;;; TESTS
(begin-for-test
  (check-equal? (new-racket W-NO-COLL) (make-racket 350 375 0 10 false 0 0)
                "new-racket test 1 did not calculate correctly"))


;;; new-racket-x : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the x-coordinate of the racket after 1 tick
;;; EXAMPLES:
;;; (new-racket-x B-RTS R-COLLIDE-LEFT) => 24
;;; (new-racket-x B-RTS R-COLLIDE-RIGHT) => 401
;;; (new-racket-x B-RTS R-RTS) => 330
;;; DESIGN STRATEGY: Cases on racket collisions, then transcribe formula

(define (new-racket-x b r)
  (cond [(racket-collide-left-side? r) (+ RACKET-HALF 1)]
        [(racket-collide-right-side? r) (- COURT-WIDTH RACKET-HALF 1)]
        [(racket-selected? r) (racket-x r)]
        [else (+ (racket-x r) (racket-vx r))]))

;;; TESTS
(begin-for-test
  (check-equal? (new-racket-x B-RTS R-COLLIDE-LEFT) 24
                "new-racket-x test 1 did not calculate correctly")
  (check-equal? (new-racket-x B-RTS R-COLLIDE-RIGHT) 401
                "new-racket-x test 2 did not calculate correctly")
  (check-equal? (new-racket-x B-RTS R-RTS) 330
                "new-racket-x test 3 did not calculate correctly"))


;;; new-racket-y : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the y-coordinate of the racket after 1 tick
;;; EXAMPLES:
;;; (new-racket-y B-COLLIDE-R R-COLLIDE-B) => 365
;;; (new-racket-y B-RTS R-RTS) => 384
;;; DESIGN STRATEGY: Cases on racket collisions, then transcribe formula

(define (new-racket-y b r)
  (cond [(ball-collide-racket? b r) (+ (racket-y r) (racket-vy r))]
        [(racket-selected? r) (racket-y r)]
        [else (+ (racket-y r) (racket-vy r))]))

;;; TESTS
(begin-for-test
  (check-equal? (new-racket-y B-COLLIDE-R R-COLLIDE-B) 365
                "new-racket-y test 1 did not calculate correctly")
  (check-equal? (new-racket-y B-RTS R-RTS) 384
                "new-racket-y test 2 did not calculate correctly"))


;;; new-racket-vx : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the x velocity of the racket after 1 tick
;;; EXAMPLES:
;;; (new-racket-vx B-RTS R-COLLIDE-LEFT) => 0
;;; (new-racket-vx B-RTS R-COLLIDE-RIGHT) => 0
;;; (new-racket-vx B-RTS R-RTS) => 0
;;; DESIGN STRATEGY: Cases on racket collisions, then transcribe formula

(define (new-racket-vx b r)
  (cond [(racket-collide-left-side? r) 0]
        [(racket-collide-right-side? r) 0]
        [else (racket-vx r)]))

;;; TESTS
(begin-for-test
  (check-equal? (new-racket-vx B-RTS R-COLLIDE-LEFT) 0
                "new-racket-vx test 1 did not calculate correctly")
  (check-equal? (new-racket-vx B-RTS R-COLLIDE-RIGHT) 0
                "new-racket-vx test 2 did not calculate correctly")
  (check-equal? (new-racket-vx B-RTS R-RTS) 0
                "new-racket-vx test 3 did not calculate correctly"))


;;; new-racket-vy : Ball Racket -> Integer
;;; GIVEN: a ball and a racket
;;; RETURNS: the y-coordinate of the racket after 1 tick
;;; EXAMPLES:
;;; (new-racket-vy B-COLLIDE-R R-COLLIDE-B) => 0
;;; (new-racket-vy B-COLLIDE-R (make-racket 350 365 5 -1 false)) => 0
;;; (new-racket-vy B-RTS R-RTS) => 0
;;; DESIGN STRATEGY: Cases on racket collisions, then transcribe formula

(define (new-racket-vy b r)
  (cond [(ball-collide-racket? b r) (if (< (racket-vy r) 0) 0 (racket-vy r))]
        [else (racket-vy r)]))

;;; TESTS
(begin-for-test
  (check-equal? (new-racket-vy B-COLLIDE-R R-COLLIDE-B) 0
                "new-racket-vy test 1 did not calculate correctly")
  (check-equal? (new-racket-vy B-COLLIDE-R (make-racket 350 365 5 -1 false 0 0))
                0
                "new-racket-vy test 2 did not calculate correctly")
  (check-equal? (new-racket-vy B-RTS R-RTS) 0
                "new-racket-vy test 3 did not calculate correctly"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; COLLISION CHECKS

;;; ball-collide-front? : Ball -> Boolean
;;; GIVEN: a ball
;;; RETURNS: true if the ball collides with the front wall
;;; EXAMPLE:
;;; (ball-collide-front? B-COLLIDE-TOP) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (ball-collide-front? b)
  (< (+ (ball-y b) (ball-vy b)) 0))

;;; TESTS
(begin-for-test
  (check-equal? (ball-collide-front? B-COLLIDE-TOP) true
                "ball-collide-front? test 1 did not calculate correctly"))


;;; ball-collide-left-side? : Ball -> Boolean
;;; GIVEN: a ball
;;; RETURNS: true if the ball collides with the left wall
;;; EXAMPLE:
;;; (ball-collide-left-side? B-COLLIDE-LEFT) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (ball-collide-left-side? b)
  (< (+ (ball-x b) (ball-vx b)) 0))

;;; TESTS
(begin-for-test
  (check-equal? (ball-collide-left-side? B-COLLIDE-LEFT) true
                "ball-collide-left-side? test 1 did not calculate correctly"))


;;; ball-collide-right-side? : Ball -> Boolean
;;; GIVEN: a ball
;;; RETURNS: true if the ball collides with the right wall
;;; EXAMPLE:
;;; (ball-collide-right-side? B-COLLIDE-RIGHT) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (ball-collide-right-side? b)
  (> (+ (ball-x b) (ball-vx b)) COURT-WIDTH))

;;; TESTS
(begin-for-test
  (check-equal? (ball-collide-right-side? B-COLLIDE-RIGHT) true
                "ball-collide-right-side? test 1 did not calculate correctly"))


;;; ball-collide-back? : Ball -> Boolean
;;; GIVEN: a ball
;;; RETURNS: true if the ball collides with the back wall
;;; EXAMPLE:
;;; (ball-collide-back? B-COLLIDE-BOTTOM) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (ball-collide-back? b)
  (> (+ (ball-y b) (ball-vy b)) COURT-HEIGHT))

;;; TESTS
(begin-for-test
  (check-equal? (ball-collide-back? B-COLLIDE-BOTTOM) true
                "ball-collide-back? test 1 did not calculate correctly"))


;;; ball-collide-racket? : Ball Racket -> Boolean
;;; GIVEN: a ball and a racket
;;; RETURNS: true if the ball collides with the racket
;;; EXAMPLE:
;;; (ball-collide-racket? B-COLLIDE-R R-COLLIDE-B) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (ball-collide-racket? b r)
  (or
   (and (> (+ (ball-y b) (ball-vy b))
           (+ (racket-y r) (racket-vy r)))
        (> (racket-y r) (ball-y b))
        (ball-collide-racket-width? b r))
   (and (< (+ (ball-y b) (ball-vy b))
           (+ (racket-y r) (racket-vy r)))
        (< (racket-y r) (ball-y b))
        (ball-collide-racket-width? b r))))

;;; TESTS
(begin-for-test
  (check-equal? (ball-collide-racket? B-COLLIDE-R R-COLLIDE-B) true
                "ball-collide-racket? test 1 did not calculate correctly")
  (check-equal? (ball-collide-racket? (make-ball 350 365 0 -10)
                                      (make-racket 350 360 5 0 false 0 0))
                true
                "ball-collide-racket? test 2 did not calculate correctly"))

;;; ball-collide-racket-width? : Ball Racket -> Boolean
;;; GIVEN: a ball and a racket
;;; RETURNS: true if the ball collides with the racket width
;;; EXAMPLE:
;;; (ball-collide-racket-width? B-COLLIDE-R R-COLLIDE-B) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (ball-collide-racket-width? b r)
  (and (< (+ (ball-x b) (ball-vx b))
          (+ (+ (racket-x r) (racket-vx r)) RACKET-HALF))
       (> (+ (ball-x b) (ball-vx b))
          (- (+ (racket-x r) (racket-vx r)) RACKET-HALF))))

;;; TESTS
(begin-for-test
  (check-equal? (ball-collide-racket-width? B-COLLIDE-R R-COLLIDE-B) true
                "ball-collide-racket-width? test 1 not correct"))

;;; racket-collide-front? : Racket -> Boolean
;;; GIVEN: a racket
;;; RETURNS: true if the ball collides with the racket
;;; EXAMPLE:
;;; (racket-collide-front? R-COLLIDE-TOP) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (racket-collide-front? r)
  (< (+ (racket-y r) (racket-vy r)) 0))

;;; TESTS
(begin-for-test
  (check-equal? (racket-collide-front? R-COLLIDE-TOP) true
                "racket-collide-front? test 1 did not calculate correctly"))

;;; racket-collide-left-side? : Racket -> Boolean
;;; GIVEN: a racket
;;; RETURNS: true if the racket collides with a side wall
;;; EXAMPLE:
;;; (racket-collide-left-side? R-COLLIDE-LEFT) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (racket-collide-left-side? r)
  (< (- (+ (racket-x r) (racket-vx r)) RACKET-HALF) 0))

;;; TESTS
(begin-for-test
  (check-equal? (racket-collide-left-side? R-COLLIDE-LEFT) true
                "racket-collide-left-side? test 1 did not calculate correctly"))

;;; racket-collide-right-side? : Racket -> Boolean
;;; GIVEN: a racket
;;; RETURNS: true if the racket collides with a side wall
;;; EXAMPLE:
;;; (racket-collide-right-side? R-COLLIDE-RIGHT) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (racket-collide-right-side? r)
  (> (+ (+ (racket-x r) (racket-vx r)) RACKET-HALF) COURT-WIDTH))

;;; TESTS
(begin-for-test
  (check-equal? (racket-collide-right-side? R-COLLIDE-RIGHT) true
                "racket-collide-right-side? test 1 not correct"))

;;; racket-collide-back? : Racket -> Boolean
;;; GIVEN: a racket
;;; RETURNS: true if the ball collides with the racket
;;; EXAMPLE:
;;; (racket-collide-back? R-COLLIDE-BOTTOM) => true
;;; DESIGN STRATEGY:  Combine simpler functions

(define (racket-collide-back? r)
  (> (+ (racket-y r) (racket-vy r)) COURT-HEIGHT))

;;; TESTS
(begin-for-test
  (check-equal? (racket-collide-back? R-COLLIDE-BOTTOM) true
                "racket-collide-back? test 1 did not calculate correctly"))

;;; END COLLISION CHECKS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; KEY EVENTS
            
;;; world-after-key-event : World KeyEvent -> World
;;; GIVEN: a world and a key event
;;; RETURNS: the world that should follow the given world
;;;     after the given key event
;;; EXAMPLES:
;;; (world-after-key-event W-RTS " ") => (world-after-rts-key-event W-RTS " ")
;;; (world-after-key-event W-NO-COLL " ") =>
;;;                                (world-after-rally-key-event W-NO-COLL " ")
;;; DESIGN STRATEGY:  Cases on ready-to-serve? and cases on key event

(define (world-after-key-event w kev)
  (if (world-ready-to-serve? w)
      (world-after-rts-key-event w kev)
      (world-after-rally-key-event w kev)))

;;;TESTING
(begin-for-test
  (check-equal? (world-after-key-event W-RTS " ")
                (world-after-rts-key-event W-RTS " ")
                "world-after-key-event test 1 did not calculate correctly")
  (check-equal? (world-after-key-event W-NO-COLL " ")
                (world-after-rally-key-event W-NO-COLL " ")
                "world-after-key-event test 2 did not calculate correctly"))


;;; world-after-rts-key-event : World KeyEvent -> World
;;; GIVEN: a ready-to-serve world and a key event
;;; RETURNS: the world that should follow the given world
;;;     after the given key event
;;; EXAMPLES:
;;; (world-after-rts-key-event W-RTS " ") => (ready-to-rally W-RTS)
;;; (world-after-rts-key-event W-RTS "left") => W-RTS
;;; DESIGN STRATEGY:  Cases on key event

(define (world-after-rts-key-event w kev)
  (if (key=? kev " ") (ready-to-rally w) w))

;;;TESTING
(begin-for-test
  (check-equal? (world-after-rts-key-event W-RTS " ") (ready-to-rally W-RTS)
                "world-after-rts-key-event test 1 did not calculate correctly")
  (check-equal? (world-after-rts-key-event W-RTS "left") W-RTS
                "world-after-rts-key-event test 2 did not calculate correctly"))


;;; world-after-rally-key-event : World KeyEvent -> World
;;; GIVEN: a rally world and a key event
;;; RETURNS: the world that should follow the given world
;;;     after the given key event
;;; EXAMPLES:
;;; (world-after-rally-key-event W-TEST " ") => (world-reset W-TEST)
;;; (world-after-rally-key-event W-TEST "left") => (world-left-arrow W-TEST)
;;; (world-after-rally-key-event W-TEST "right") => (world-right-arrow W-TEST)
;;; (world-after-rally-key-event W-TEST "up") => (world-up-arrow W-TEST)
;;; (world-after-rally-key-event W-TEST "down") => (world-down-arrow W-TEST)
;;; DESIGN STRATEGY:  Cases on key event

(define (world-after-rally-key-event w kev)
  (cond [(key=? kev " ") (world-reset w)]
            [(key=? kev "left") (world-left-arrow w)]
            [(key=? kev "right") (world-right-arrow w)]
            [(key=? kev "up") (world-up-arrow w)]
            [(key=? kev "down") (world-down-arrow w)]))

;;;TESTING
(begin-for-test
  (check-equal? (world-after-rally-key-event W-TEST " ")
                (world-reset W-TEST)
                "world-after-rally-key-event test 1 not correct")
  (check-equal? (world-after-rally-key-event W-TEST "left")
                (world-left-arrow W-TEST)
                "world-after-rally-key-event test 2 not correct")
  (check-equal? (world-after-rally-key-event W-TEST "right")
                (world-right-arrow W-TEST)
                "world-after-rally-key-event test 3 not correct")
  (check-equal? (world-after-rally-key-event W-TEST "up")
                (world-up-arrow W-TEST)
                "world-after-rally-key-event test 4 not correct")
  (check-equal? (world-after-rally-key-event W-TEST "down")
                (world-down-arrow W-TEST)
                "world-after-rally-key-event test 5 not correct"))


;;; ready-to-rally : World -> World
;;; GIVEN: a world in a ready-to-serve state
;;; RETURNS: a world in a rally state
;;; EXAMPLE: 
;;; (ready-to-rally W-RTS) => (make-world (make-ball 330 384 3 -9)
;;;                                       (make-racket 330 384 0 0)
;;;                                       1
;;;                                       false
;;;                                       false
;;;                                       0)
;;; DESIGN STRATEGY: Constructor template for World

(define (ready-to-rally w)
  (make-world (make-ball (ball-x (world-ball w))
                         (ball-y (world-ball w))
                         RALLY-VX
                         RALLY-VY)
              (make-racket (racket-x (world-racket w))
                           (racket-y (world-racket w))
                           (racket-vx (world-racket w))
                           (racket-vy (world-racket w))
                           false
                           0
                           0)
              (world-speed w)
              false
              false
              (world-plength w)))

;;;TESTING
(begin-for-test
  (check-equal? (ready-to-rally W-RTS)
                (make-world (make-ball 330 384 3 -9)
                            (make-racket 330 384 0 0 false 0 0)
                            1
                            false
                            false
                            0)
                "ready-to-rally test 1 not correct"))

;;; world-left-arrow : World -> World
;;; GIVEN: a world in a rally state
;;; RETURNS: that world with the racket left velocity increased by 1
;;; EXAMPLE: 
;;; (ready-to-rally W-NO-COLL) => (make-world (make-ball 330 360 0 10)
;;;                                           (make-racket 350 365 -1 10)
;;;                                           1
;;;                                           false
;;;                                           false
;;;                                           0)
;;; DESIGN STRATEGY: Constructor template for World

(define (world-left-arrow w)
  (make-world (world-ball w)
              (make-racket (racket-x (world-racket w))
                           (racket-y (world-racket w))
                           (- (racket-vx (world-racket w)) 1)
                           (racket-vy (world-racket w))
                           false
                           (racket-mx (world-racket w))
                           (racket-my (world-racket w)))
              (world-speed w)
              (world-ready-to-serve? w)
              (world-paused? w)
              (world-plength w)))

;;;TESTING
(begin-for-test
  (check-equal? (world-left-arrow W-NO-COLL)
                (make-world (make-ball 350 360 0 10)
                            (make-racket 350 365 -1 10 false 0 0)
                            1
                            false
                            false
                            0)
                "world-left-arrow test 1 not correct"))

;;; world-right-arrow : World -> World
;;; GIVEN: a world in a rally state
;;; RETURNS: that world with the racket right velocity increased by 1
;;; EXAMPLE: 
;;; (ready-to-rally W-NO-COLL) => (make-world (make-ball 330 360 0 10)
;;;                                           (make-racket 350 365 1 10)
;;;                                           1
;;;                                           false
;;;                                           false
;;;                                           0)
;;; DESIGN STRATEGY: Constructor template for World

(define (world-right-arrow w)
  (make-world (world-ball w)
              (make-racket (racket-x (world-racket w))
                           (racket-y (world-racket w))
                           (+ (racket-vx (world-racket w)) 1)
                           (racket-vy (world-racket w))
                           false
                           (racket-mx (world-racket w))
                           (racket-my (world-racket w)))
              (world-speed w)
              (world-ready-to-serve? w)
              (world-paused? w)
              (world-plength w)))

;;;TESTING
(begin-for-test
  (check-equal? (world-right-arrow W-NO-COLL)
                (make-world (make-ball 350 360 0 10)
                            (make-racket 350 365 1 10 false 0 0)
                            1
                            false
                            false
                            0)
                "world-right-arrow test 1 not correct"))

;;; world-up-arrow : World -> World
;;; GIVEN: a world in a rally state
;;; RETURNS: that world with the racket up velocity increased by 1
;;; EXAMPLE: 
;;; (ready-to-rally W-NO-COLL) => (make-world (make-ball 330 360 0 10)
;;;                                           (make-racket 350 365 0 9)
;;;                                           1
;;;                                           false
;;;                                           false
;;;                                           0)
;;; DESIGN STRATEGY: Constructor template for World

(define (world-up-arrow w)
  (make-world (world-ball w)
              (make-racket (racket-x (world-racket w))
                           (racket-y (world-racket w))
                           (racket-vx (world-racket w))
                           (- (racket-vy (world-racket w)) 1)
                           false
                           (racket-mx (world-racket w))
                           (racket-my (world-racket w)))
              (world-speed w)
              (world-ready-to-serve? w)
              (world-paused? w)
              (world-plength w)))

;;;TESTING
(begin-for-test
  (check-equal? (world-up-arrow W-NO-COLL)
                (make-world (make-ball 350 360 0 10)
                            (make-racket 350 365 0 9 false 0 0)
                            1
                            false
                            false
                            0)
                "world-up-arrow test 1 not correct"))

;;; world-down-arrow : World -> World
;;; GIVEN: a world in a rally state
;;; RETURNS: that world with the racket down velocity increased by 1
;;; EXAMPLE: 
;;; (ready-to-rally W-NO-COLL) => (make-world (make-ball 330 360 0 10)
;;;                                           (make-racket 350 365 0 11)
;;;                                           1
;;;                                           false
;;;                                           false
;;;                                           0)
;;; DESIGN STRATEGY: Constructor template for World

(define (world-down-arrow w)
  (make-world (world-ball w)
              (make-racket (racket-x (world-racket w))
                           (racket-y (world-racket w))
                           (racket-vx (world-racket w))
                           (+ (racket-vy (world-racket w)) 1)
                           false
                           (racket-mx (world-racket w))
                           (racket-my (world-racket w)))
              (world-speed w)
              (world-ready-to-serve? w)
              (world-paused? w)
              (world-plength w)))

;;;TESTING
(begin-for-test
  (check-equal? (world-down-arrow W-NO-COLL)
                (make-world (make-ball 350 360 0 10)
                            (make-racket 350 365 0 11 false 0 0)
                            1
                            false
                            false
                            0)
                "world-down-arrow test 1 not correct"))

;;; END KEY EVENTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MOUSE EVENTS

;;; world-after-mouse-event : World Int Int MouseEvent -> World
;;; GIVEN: a world, the x and y coordinates of a mouse event,
;;;     and the mouse event
;;; RETURNS: the world that should follow the given world after
;;;     the given mouse event
;;; EXAMPLES:  See tests below
;;; DESIGN STRATEGY: Use constructor template for World

(define (world-after-mouse-event w mx my mev)
  (if (world-ready-to-serve? w)
      w
      (make-world (world-ball w)
                  (racket-after-mouse-event (world-racket w) mx my mev)
                  (world-speed w)
                  (world-ready-to-serve? w)
                  (world-paused? w)
                  (world-plength w))))

;;; TESTING
(begin-for-test
  (check-equal? (world-after-mouse-event W-NO-COLL 350 350 "button-down")
                (make-world (make-ball 350 360 0 10)
                            (make-racket 350 365 0 10 true 0 15)
                            1
                            false
                            false
                            0)
                "world-after-mouse-event test 1 not correct")
  (check-equal? (world-after-mouse-event W-RTS 350 350 "button-down")
                (make-world (make-ball 330 384 0 0)
                            (make-racket 330 384 0 0 false 0 0)
                            1
                            true
                            false
                            0)
                "world-after-mouse-event test 2 not correct"))
  
;;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;;     and the mouse event
;;; RETURNS: the racket as it should be after the given mouse event
;;; EXAMPLE:
;;; (racket-after-mouse-event R-RTS 330 380 "button-down") =>
;;;                                     (make-racket 330 384 0 0 true 330 380)
;;; (racket-after-mouse-event R-RTS 330 380 "drag") => R-RTS
;;; (racket-after-mouse-event R-RTS 330 380 "button-up") => R-RTS
;;; (racket-after-mouse-event R-RTS 330 380 "leave") => R-RTS
;;; DESIGN STRATEGY: Cases on mouse event

(define (racket-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (racket-after-button-down r mx my)]
    [(mouse=? mev "drag") (racket-after-drag r mx my)]
    [(mouse=? mev "button-up") (racket-after-button-up r mx my)]
    [else r]))

;;; TESTING
(begin-for-test
  (check-equal? (racket-after-mouse-event R-RTS 330 380 "button-down")
                (make-racket 330 384 0 0 true 0 4)
                "racket-after-mouse-event test 1 not correct")
  (check-equal? (racket-after-mouse-event R-RTS 330 380 "drag") R-RTS
                "racket-after-mouse-event test 2 not correct")
  (check-equal? (racket-after-mouse-event R-RTS 330 380 "button-up") R-RTS
                "racket-after-mouse-event test 3 not correct")
  (check-equal? (racket-after-mouse-event R-RTS 330 380 "leave") R-RTS
                "racket-after-mouse-event test 4 not correct"))

;;; racket-after-button-down : Racket Integer Integer -> Racket
;;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;;     and the mouse event
;;; RETURNS: the racket following a button-down at the given location.
;;; EXAMPLES:
;;; (racket-after-button-down R-RTS 330 380) =>
;;;                                     (make-racket 330 384 0 0 true 330 380)
;;; (racket-after-button-down R-RTS 330 350) => R-RTS
;;; DESIGN STRATEGY: Use constructor template for Racket

(define (racket-after-button-down r mx my)
  (if (in-racket? r mx my)
      (make-racket (racket-x r) (racket-y r) (racket-vx r)
                   (racket-vy r) true (- (racket-x r) mx) (- (racket-y r) my))
      r))

;;; TESTING
(begin-for-test
  (check-equal? (racket-after-button-down R-RTS 330 380)
                (make-racket 330 384 0 0 true 0 4)
                "racket-after-button-down test 1 not correct")
  (check-equal? (racket-after-button-down R-RTS 330 350) R-RTS
                "racket-after-button-down test 2 not correct"))
                

;;; racket-after-drag : Racket Integer Integer -> Racket
;;; GIVEN: a racket and the x and y coordinates of a mouse event
;;; RETURNS: the racket following a drag at the given location
;;; EXAMPLES:
;;; (racket-after-drag R-RTS 330 380) => (make-racket 330 384 0 0 false 0 0)
;;; (racket-after-drag R-RTS 330 350) => R-RTS                                  
;;; DESIGN STRATEGY: Use constructor template for Racket

(define (racket-after-drag r mx my)
  (if (racket-selected? r)
      (make-racket (+ (racket-mx r) mx)
                   (+ (racket-my r) my)
                    (racket-vx r) (racket-vy r) true
                    (racket-mx r) (racket-my r))
      r))

;;; TESTING
(begin-for-test
  (check-equal? (racket-after-drag (make-racket 330 384 0 0 true 0 0) 330 380)
                (make-racket 330 380 0 0 true 0 0)
                "racket-after-drag test 1 not correct")
  (check-equal? (racket-after-drag R-RTS 330 350) R-RTS
                "racket-after-drag test 2 not correct"))

;;; racket-after-button-up : Racket Integer Integer -> Racket
;;; GIVEN: a racket and the x and y coordinates of a mouse event
;;; RETURNS: the racket following a button-up at the given location
;;; EXAMPLES:
;;; (racket-after-button-up (make-racket 330 384 0 0 true 330 380) 330 380) =>
;;;                                          (make-racket 330 384 0 0 false 0 0)
;;; (racket-after-button-up R-RTS 330 350) => R-RTS
;;; DESIGN STRATEGY: Use constructor template for Racket

(define (racket-after-button-up r mx my)
  (if (racket-selected? r)
      (make-racket (racket-x r) (racket-y r) (racket-vx r)
                   (racket-vy r) false 0 0)
      r))

;;; TESTING
(begin-for-test
  (check-equal? (racket-after-button-up
                 (make-racket 330 384 0 0 true 330 380) 330 380)
                (make-racket 330 384 0 0 false 0 0)
                "racket-after-button-up test 1 not correct")
  (check-equal? (racket-after-button-up R-RTS 330 350) R-RTS
                "racket-after-button-up test 2 not correct"))

;;; in-racket? : Racket Integer Integer -> Boolean
;;; GIVEN: a racket and the x and y coordinates of a mouse event
;;; RETURNS: true if the racket is within mouse range
;;; EXAMPLES:
;;; (in-racket? R-RTS 330 300) => false
;;; DESIGN STRATEGY: Transcribe formula
(define (in-racket? r mx my)
  (and
    (<= 
      (- (racket-x r) 25)
      mx
      (+ (racket-x r) 25))
    (<= 
      (- (racket-y r) 25)
      my
      (+ (racket-y r) 25))))

;;; TESTING
(begin-for-test
  (check-equal? (in-racket? R-RTS 330 300) false
                "in-racket? test 1 not correct"))