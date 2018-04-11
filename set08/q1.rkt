;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide tie
         defeated
         defeated?
         outranks
         outranked-by)

(check-location "08" "q1.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS


;;; A Competitor is represented as a String (any string will do).


;;; A CompetitorList is represented as a list of Competitors

;;; CONSTRUCTOR TEMPLATES:
;;; -- empty              represents 0
;;; -- (cons comp cl)
;;;         WHERE comp is a Competitor and cl is a CompetitorList

;;; OBSERVER TEMPLATE:

;;; clist-fn : CompetitorList -> ??
#;
(define (clist-fn clist)
  (cond [(empty? clist) ...]
        [else (...
               (first clist)
               (clist-fn (rest clist)))]))


;;; A Tie is represented as a
;;;    (make-tie-type comp1 comp2)
;;; with the following fields:
;;; comp1 : Competitor   the competitor that tied comp2
;;; comp2 : Competitor   the competitor that tied comp1

;;; IMPLEMENTATION
(define-struct tie-type (comp1 comp2))

;;; CONSTRUCTOR TEMPLATE
;;; A Tie is a (make-tie-type Competitor Competitor)

;;; OBSERVER TEMPLATE
;;; tie-type-fn : Tie -> ??
#;
(define (tie-type-fn t)
  (... (tie-type-comp1 t)
       (tie-type-comp2 t)))


;;; A Defeat is represented as a
;;;    (make-defeat-type winner loser)
;;; with the following fields:
;;; winner : Competitor   the competitor that beat loser
;;;  loser : Competitor   the competitor that lost to winner

;;; IMPLEMENTATION
(define-struct defeat-type (winner loser))

;;; CONSTRUCTOR TEMPLATE
;;; A Defeat is a (make-defeat-type Competitor Competitor)

;;; OBSERVER TEMPLATE
;;; defeat-type-fn : Defeat -> ??
#;
(define (defeat-type-fn d)
  (... (defeat-type-comp1 d)
       (defeat-type-comp2 d)))


;;; An Outcome is one of
;;;     -- a Tie
;;;     -- a Defeat
;;;
;;; OBSERVER TEMPLATE:
;;; outcome-fn : Outcome -> ??
#;
(define (outcome-fn o)
  (cond ((tie? o) ...)
        ((defeat? o) ...)))


;;; A OutcomeList is represented as a list of Outcomes

;;; CONSTRUCTOR TEMPLATES:
;;; -- empty              represents 0
;;; -- (cons o ol)
;;;         WHERE o is an Outcome and ol is an OutcomeList

;;; OBSERVER TEMPLATE:

;;; olist-fn : OutcomeList -> ??
#;
(define (olist-fn olist)
  (cond [(empty? olist) ...]
        [else (...
               (first olist)
               (olist-fn (rest olist)))]))


;;; A CompetitorHistory is represented as a
;;;    (make-comp-history (name outranks outranked))
;;; with the following fields:
;;;      name : Competitor       the name of the competitor
;;;  outranks : CompetitorList   the competitors that this competitor outranks
;;; outranked : CompetitorLIst   the competitors that outrank this competitor

;;; IMPLEMENTATION
(define-struct comp-history (name outranks outranked))

;;; CONSTRUCTOR TEMPLATE
;;; A CompetitorHistory is a
;;;      (make-comp-history Competitor CompetitorList CompetitorList)

;;; OBSERVER TEMPLATE
;;; comp-history-fn : CompetitorHistory -> ??
#;
(define (comp-history-fn ch)
  (... (comp-history-name ch)
       (comp-history-outranks ch)
       (comp-history-outranked ch)))


;;; A CompetitorHistoryList is represented as a list of CompetitorHistorys

;;; CONSTRUCTOR TEMPLATES:
;;; -- empty              represents 0
;;; -- (cons ch chlist)
;;;        WHERE ch is a CompetitorHistory and chlist is a CompetitorHistoryList

;;; OBSERVER TEMPLATE:

;;; chlist-fn : CompetitorHistoryList -> ??
#;
(define (chlist-fn chlist)
  (cond [(empty? chlist) ...]
        [else (...
               (first chlist)
               (clist-fn (rest chlist)))]))


;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FUNCTION DEFINITIONS

;;; tie : Competitor Competitor -> Tie
;;; GIVEN: the names of two competitors
;;; RETURNS: an indication that the two competitors have
;;;     engaged in a contest, and the outcome was a tie
;;; EXAMPLE: (see the examples given below for defeated?,
;;;     which shows the desired combined behavior of tie
;;;     and defeated?)
;;; DESIGN STRATEGY: Use constructor template for tie

(define (tie comp1 comp2)
  (make-tie-type comp1 comp2))


;;; defeated : Competitor Competitor -> Defeat
;;; GIVEN: the names of two competitors
;;; RETURNS: an indication that the two competitors have
;;;     engaged in a contest, with the first competitor
;;;     defeating the second
;;; EXAMPLE: (see the examples given below for defeated?,
;;;     which shows the desired combined behavior of defeated
;;;     and defeated?)
;;; DESIGN STRATEGY: Use contructor template for defeat

(define (defeated comp1 comp2)
  (make-defeat-type comp1 comp2))


;;; defeated? : Competitor Competitor OutcomeList -> Boolean
;;; GIVEN: the names of two competitors and a list of outcomes
;;; RETURNS: true if and only if one or more of the outcomes indicates
;;;     the first competitor has defeated or tied the second
;;; EXAMPLES:
;;;     (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
;;;  => true
;;;
;;;     (defeated? "A" "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => false
;;;
;;;     (defeated? "B" "A" (list (defeated "A" "B") (tie "B" "C")))
;;;  => false
;;;
;;;     (defeated? "B" "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => true
;;;
;;;     (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
;;;  => true
;;; DESIGN STRATEGY : Use HOF ormap

(define (defeated? comp1 comp2 olist)
  (ormap (defeat-conditions? comp1 comp2) olist))

;;; TESTS
(begin-for-test
  (check-equal? (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
                true
                "defeated? test 1 not correct")
  (check-equal? (defeated? "A" "C" (list (defeated "A" "B") (tie "B" "C")))
                false
                "defeated? test 2 not correct")
  (check-equal? (defeated? "B" "A" (list (defeated "A" "B") (tie "B" "C")))
                false
                "defeated? test 3 not correct")
  (check-equal? (defeated? "B" "C" (list (defeated "A" "B") (tie "B" "C")))
                true
                "defeated? test 4 not correct")
  (check-equal? (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
                true
                "defeated? test 5 not correct"))


;;; defeat-conditions? : Competitor Competitor -> Boolean
;;; GIVEN: the names of two competitors
;;; RETURNS: true if the first competitor has defeated or tied the second
;;; DESIGN STRATEGY: Use observer template for an Outcome

(define (defeat-conditions? comp1 comp2)
  (lambda(outcome)
    (cond [(defeat-type? outcome) (defeat-conditions-def? comp1 comp2 outcome)]
          [(tie-type? outcome) (defeat-conditions-tie? comp1 comp2 outcome)])))

;;; TESTS
(begin-for-test
  (check-equal? (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
                true
                "defeated? test 1 not correct"))


;;; defeat-conditions-def? : Competitor Competitor Defeat -> Boolean
;;; GIVEN: the names of two competitors and an arbitrary defeat
;;; RETURNS: true if the first competitor is the winner and the second
;;;          competitor is the loser of the given defeat
;;; DESIGN STRATEGY: Combine simpler functions

(define (defeat-conditions-def? comp1 comp2 o)
  (and (string=? comp1 (defeat-type-winner o))
       (string=? comp2 (defeat-type-loser o))))

;;; TESTS
(begin-for-test
  (check-equal? (defeat-conditions-def? "A" "B" (defeated "A" "B"))
                true
                "defeat-conditions-def? test 1 not correct"))


;;; defeat-conditions-tie? : Competitor Competitor Tie -> Boolean
;;; GIVEN: the names of two competitors and an arbitrary tie
;;; RETURNS: true if the two competitor names match the names of the given
;;;          tie
;;; DESIGN STRATEGY: Combine simpler functions

(define (defeat-conditions-tie? comp1 comp2 o)
  (or (and (string=? comp1 (tie-type-comp1 o))
                    (string=? comp2 (tie-type-comp2 o)))
               (and (string=? comp2 (tie-type-comp1 o))
                    (string=? comp1 (tie-type-comp2 o)))))

;;; TESTS
(begin-for-test
  (check-equal? (defeat-conditions-tie? "A" "B" (tie "B" "A"))
                true
                "defeat-conditions-tie? test 1 not correct"))


;;; outranks : Competitor OutcomeList -> CompetitorList
;;; GIVEN: the name of a competitor and a list of outcomes
;;; RETURNS: a list of the competitors outranked by the given
;;;     competitor, in alphabetical order
;;; NOTE: it is possible for a competitor to outrank itself
;;; EXAMPLES:
;;;     (outranks "A" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list "B" "C")
;;;
;;;     (outranks "B" (list (defeated "A" "B") (defeated "B" "A")))
;;;  => (list "A" "B")
;;;
;;;     (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list "B" "C")
;;; DESIGN STRATEGY: Use HOF filter to return the given Competitor's outranks
;;;                  list

(define (outranks comp olist)
  (comp-history-outranks
   (first (filter
           ;; CompetitorHistory -> Competitor
           (lambda(x) (string=? (comp-history-name x) comp))
                  (outcome-cycler olist)))))

;;; TESTS
(begin-for-test
  (check-equal? (outranks "A" (list (defeated "A" "B") (tie "B" "C")))
                (list "B" "C")
                "outranks test 1 not correct")
  (check-equal? (outranks "B" (list (defeated "A" "B") (defeated "B" "A")))
                (list "A" "B")
                "outranks test 2 not correct")
  (check-equal? (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "B" "C")
                "outranks test 3 not correct"))


;;; outranked-by : Competitor OutcomeList -> CompetitorList
;;; GIVEN: the name of a competitor and a list of outcomes
;;; RETURNS: a list of the competitors that outrank the given
;;;     competitor, in alphabetical order
;;; NOTE: it is possible for a competitor to outrank itself
;;; EXAMPLES:
;;;     (outranked-by "A" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list)
;;;
;;;     (outranked-by "B" (list (defeated "A" "B") (defeated "B" "A")))
;;;  => (list "A" "B")
;;;
;;;     (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list "A" "B" "C")
;;; DESIGN STRATEGY: Use HOF filter to return the given Competitor's outranked
;;;                  list

(define (outranked-by comp olist)
  (comp-history-outranked
   (first (filter
           ;; CompetitorHistory -> Competitor
           (lambda(x) (string=? (comp-history-name x) comp))
                  (outcome-cycler olist)))))

;;; TESTS
(begin-for-test
  (check-equal? (outranked-by "A" (list (defeated "A" "B") (tie "B" "C")))
                (list)
                "outranked-by test 1 not correct")
  (check-equal? (outranked-by "B" (list (defeated "A" "B") (defeated "B" "A")))
                (list "A" "B")
                "outranked-by test 2 not correct")
  (check-equal? (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "A" "B" "C")
                "outranked-by test 3 not correct")
  (check-equal? (outranked-by "A" (list (defeated "B" "C")
                                        (defeated "D" "E")
                                        (tie "C" "D")
                                        (defeated "A" "B")
                                        (defeated "D" "A")))
                (list "A" "B" "C" "D")
                "outranked-by test 4 not correct")
  (check-equal? (outranked-by "B" (list (defeated "C" "B")))
                (list "C")
                "outranked-by test 5 not correct")
  (check-equal? (outranked-by "C" (list (defeated "C" "B")))
                (list)
                "outranked-by test 6 not correct")
  (check-equal? (outranked-by "C" (list (defeated "C" "B")
                                        (defeated "A" "C")
                                        (defeated "C" "D")
                                        (defeated "D" "E")))
                (list "A")
                "outranked-by test 7 not correct"))


;;; outcome-cycler : OutcomeList -> CompetitorHistoryList
;;; GIVEN: a list of outcomes
;;; RETURNS: the competitor history list with all outcomes recorded
;;; DESIGN STRATEGY: Use HOF foldl to record all outcomes

(define (outcome-cycler olist)
  (competitor-history-cycler
   (foldl
    ;; Outcome CompetitorHistoryList -> CompetitorHistoryList
    (lambda (o chlist) (insert-outcome o chlist)) empty olist) -1))

;;; TESTS
(begin-for-test
  (check-equal? (outcome-cycler (list (defeated "A" "B") (tie "B" "C")))
                (list (make-comp-history "C" (list "B" "C") (list "A" "B" "C"))
                      (make-comp-history "B" (list "B" "C") (list "A" "B" "C"))
                      (make-comp-history "A" (list "B" "C") (list)))
                "outcome-cycler test 1 not correct")
  (check-equal? (outcome-cycler (list (defeated "C" "B")
                                      (defeated "A" "C")
                                      (defeated "C" "D")
                                      (defeated "D" "E")))
                (list (make-comp-history "E" (list) (list "A" "C" "D"))
                      (make-comp-history "D" (list "E") (list "A" "C"))
                      (make-comp-history "A" (list "B" "C" "D" "E") (list))
                      (make-comp-history "B" (list) (list "A" "C"))
                      (make-comp-history "C" (list "B" "D" "E") (list "A")))
                "outcome-cycler test 2 not correct"))


;;; insert-outcome : Outcome CompetitorHistoryList -> CompetitorHistoryList
;;; GIVEN: an outcome and a competitor history list
;;; WHERE: the competitor history list represents all outcomes recorded thus far
;;; RETURNS: the competitor history list with the given outcome recorded
;;; DESIGN STRATEGY: Use observer template for Outcome

(define (insert-outcome o chlist)
  (if (defeat-type? o)
      (insert-defeat (defeat-type-winner o) (defeat-type-loser o) chlist)
      (insert-tie (tie-type-comp1 o) (tie-type-comp2 o) chlist)))

;;; TESTS
(begin-for-test
  (check-equal? (insert-outcome (defeated "A" "B") empty)
                (list (make-comp-history "B" (list) (list "A"))
                      (make-comp-history "A" (list "B") (list)))
                "insert-outcome test 1 not correct"))


;;; competitor-history-cycler : CompetitorHistoryList Int
;;;                                                     -> CompetitorHistoryList
;;; GIVEN: a competitor history list and an int
;;; WHERE: the competitor history list has all outcomes recorded and the count
;;;        represents the number of iterations remaining
;;; RETURNS: the competitor history list with filled outranks/outranked lists
;;; DESIGN STRATEGY: Use recursive loop to iterate through; halt when count is 0

(define (competitor-history-cycler chlist count)
  (cond [(= count 0) chlist]
        [(= count -1) (competitor-history-cycler chlist (length chlist))]
        [else (competitor-history-cycler (outranks-outranked-cycler chlist)
                                         (- count 1))]))

;;; TESTS
(begin-for-test
  (check-equal? (competitor-history-cycler
                 (list (make-comp-history "B" (list) (list "A"))
                       (make-comp-history "A" (list "B") (list))) 0)
                (list (make-comp-history "B" (list) (list "A"))
                      (make-comp-history "A" (list "B") (list)))
                "competitor-history-cycler test 1 not correct"))

;;; outranks-outranked-cycler : CompetitorHistoryList -> CompetitorHistoryList
;;; GIVEN: a competitor history list of all outcomes recorded
;;; RETURNS: the competitor history list with complete outranks/outranked lists
;;; DESIGN STRATEGY: Use HOF map to cycle through the competitor history list

(define (outranks-outranked-cycler chlist)
  (map (lambda (x) (competitor-outranks-cycler x chlist)) chlist))

;;; TESTS
(begin-for-test
  (check-equal? (outranks-outranked-cycler
                 (list (make-comp-history "B" (list) (list "A"))
                       (make-comp-history "A" (list "B") (list))))
                (list (make-comp-history "B" (list) (list "A"))
                      (make-comp-history "A" (list "B") (list)))
                "outranks-outranked-cycler test 1 not correct"))


;;; competitor-outranks-cycler : CompetitorHistory CompetitorHistoryList
;;;                                                     -> CompetitorHistory
;;; GIVEN: a competitor history and a competitor history list
;;; WHERE: the competitor history is taken from the competitor history list and
;;;        the competitor history list has all outcomes recorded
;;; RETURNS: the given competitor history with complete outranks/outranked lists
;;; DESIGN STRATEGY: Use constructor template for CompetitorHistory and HOF sort
;;;                  on the outranks and outranked lists

(define (competitor-outranks-cycler ch chlist)
  (make-comp-history (comp-history-name ch)
                     (sort (outranks-cycler ch chlist) string<?)
                     (sort (outranked-cycler ch chlist) string<?)))

;;; TESTS
(begin-for-test
  (check-equal? (competitor-outranks-cycler
                 (make-comp-history "B" (list) (list))
                 (list (make-comp-history "B" (list) (list "A"))
                       (make-comp-history "A" (list "B") (list))))
                (make-comp-history "B" (list) (list "A"))
                "competitor-outranks-cycler test 1 not correct"))

;;; outranks-cycler : CompetitorHistory CompetitorHistoryList -> CompetitorList
;;; GIVEN: a competitor history and a competitor history list
;;; WHERE: the competitor history is taken from the competitor history list and
;;;        the competitor history list has all outcomes recorded
;;; RETURNS: the complete outranks list for the given CompetitorHistory
;;; DESIGN STRATEGY: Use HOF foldl

(define (outranks-cycler ch chlist)
  (remove-dupes (foldl
                 ;; CompetitorList CompetitorList -> CompetitorList
                 (lambda (x r) (append (loser-outranks x chlist) r))
                       (comp-history-outranks ch)
                       (comp-history-outranks ch))))

;;; TESTS
(begin-for-test
  (check-equal? (outranks-cycler
                 (make-comp-history "A" (list "B") (list))
                 (list (make-comp-history "B" (list) (list "A"))
                       (make-comp-history "A" (list "B") (list))))
                (list "B")
                "outranks-cycler test 1 not correct"))

;;; outranked-cycler : CompetitorHistory CompetitorHistoryList -> CompetitorList
;;; GIVEN: a competitor history and a competitor history list
;;; WHERE: the competitor history is taken from the competitor history list and
;;;        the competitor history list has all outcomes recorded
;;; RETURNS: the complete outranked list for the given CompetitorHistory
;;; DESIGN STRATEGY: Use HOF foldl

(define (outranked-cycler ch chlist)
  (remove-dupes
   (foldl
    ;; CompetitorList CompetitorList -> CompetitorList
    (lambda (x r) (append
                         (loser-outranked (comp-history-name ch) x) r))
          (comp-history-outranked ch)
          chlist)))

;;; TESTS
(begin-for-test
  (check-equal? (outranked-cycler
                 (make-comp-history "A" (list "B") (list))
                 (list (make-comp-history "B" (list) (list "A"))
                       (make-comp-history "A" (list "B") (list))))
                (list)
                "outranked-cycler test 1 not correct"))

;;; insert-tie : Competitor Competitor CompetitorHistoryList
;;;                                                     -> CompetitorHistoryList
;;; GIVEN: two competitors and a competitor history list
;;; WHERE: the two competitors have tied in a contest and the competitor history
;;;        list represents all recorded outcomes thus far
;;; RETURNS: the list of competitor history with this tie recorded
;;; DESIGN STRATEGY: Combine simpler functions

(define (insert-tie comp1 comp2 chlist)
  (insert-defeat comp2 comp1 (insert-defeat comp1 comp2 chlist)))

;;; TESTS
(begin-for-test
  (check-equal? (insert-tie "A" "B" empty)
                (list (make-comp-history "B" (list "A" "B") (list "A"))
                      (make-comp-history "A" (list "B") (list)))
                "insert-tie test 1 not correct"))

;;; insert-defeat :
;;;         Competitor Competitor CompetitorHistoryList -> CompetitorHistoryList
;;; GIVEN: two competitors and a competitor history list
;;; WHERE: the comp1 competitor has defeated the comp2 competitor and the
;;;        competitor history list represents all recorded outcomes thus far
;;; RETURNS: the list of competitor history with this defeat recorded
;;; DESIGN STRATEGY: If comp1 is on chlist use HOF Map, else add new competitor
;;;                  to chlist

(define (insert-defeat comp1 comp2 chlist)
  (append (insert-loser comp1 comp2 chlist)
          (if (competitor-on-list? comp1 chlist)
              (map
               ;; CompetitorHistory -> CompetitorHistory
               (lambda (x) (insert-outranks comp1 comp2 chlist x)) chlist)
              (cons (new-comp-history comp1 comp2 chlist) chlist))))

;;; TESTS
(begin-for-test
  (check-equal? (insert-defeat "A" "B" empty)
                (list (make-comp-history "B" (list) (list "A"))
                      (make-comp-history "A" (list "B") (list)))
                "insert-defeat test 1 not correct"))

;;; insert-loser : Competitor Competitor CompetitorHistoryList
;;;                                                         -> CompetitorHistory
;;; GIVEN: two competitors and a competitor history list
;;; WHERE: the comp2 competitor has been defeated by the comp1 competitor and
;;;        the competitor history list represents all recorded outcomes thus far
;;; RETURNS: the competitor history for the loser with the loss recorded
;;; DESIGN STRATEGY: Constructor template for Competitor History

(define (insert-loser comp1 comp2 chlist)
  (if (competitor-on-list? comp2 chlist)
      empty
      (list (make-comp-history comp2 empty (list comp1)))))

;;; TESTS
(begin-for-test
  (check-equal? (insert-loser "A" "B" empty)
                (list (make-comp-history "B" (list) (list "A")))
                "insert-loser test 1 not correct"))

;;; insert-outranks : Competitor Competitor 
;;;                  CompetitorHistoryList CompetitorHistory-> CompetitorHistory
;;; GIVEN: two competitors comp1 and comp2, a competitor history list chlist,
;;;        and a competitor history x
;;; WHERE: comp1 has defeated comp2, chlist is the competitor history list
;;;        recorded thus far, and x is the competitor history being tested
;;; RETURNS: the competitor history list with comp2 added to comp1's outranks
;;;          list
;;; DESIGN STRATEGY: Constructor template for CompetitorHistory

(define (insert-outranks comp1 comp2 chlist x)
  (if (string=? comp1 (comp-history-name x))
      (make-comp-history (comp-history-name x)
                         (insert-outranks-list comp1 comp2 x chlist)
                         (comp-history-outranked x))
      x))

;;; TESTS
(begin-for-test
  (check-equal? (insert-outranks "A" "B" empty
                                 (make-comp-history "B" (list) (list "A")))
                (make-comp-history "B" (list) (list "A"))
                "insert-outranks test 1 not correct"))

;;; new-comp-history :
;;;             Competitor Competitor CompetitorHistoryList -> CompetitorHistory
;;; GIVEN: two competitors comp1 and comp2, and a competitor history list chlist
;;; WHERE: comp1 has defeated comp2 and chlist is the competitor history list
;;;        recorded thus far
;;; RETURNS: the competitor history list with comp1 added as a new competitor
;;;          history with comp2 in its outranks list
;;; DESIGN STRATEGY: Constructor template for CompetitorHistory

(define (new-comp-history comp1 comp2 chlist)
  (make-comp-history comp1
                     (cons comp2 (loser-outranks comp2 chlist))
                     empty))

;;; TESTS
(begin-for-test
  (check-equal? (new-comp-history "A" "B" empty)
                (make-comp-history "A" (list "B") (list))
                "new-comp-history test 1 not correct"))

;;; competitor-on-list? : Competitor CompetitorHistoryList -> Boolean
;;; GIVEN: a competitor and a competitor history list
;;; RETURNS: true if the name of the competitor is already on the list
;;; DESIGN STRATEGY: Use HOF ormap

(define (competitor-on-list? comp chlist)
  (ormap (lambda(x) (string=? comp (comp-history-name x))) chlist))

;;; TESTS
(begin-for-test
  (check-equal? (competitor-on-list? "A"
                                     (list (make-comp-history "A" (list "B")
                                                              (list))))
                true
                "competitor-on-list? test 1 not correct"))

;;; insert-outranks-list : Competitor Competitor CompetitorHistory
;;;                                      CompetitorHistoryList -> CompetitorList
;;; GIVEN: two competitors comp1 adn comp2, a competitor history ch, and a
;;;        competitor history list chlist
;;; WHERE: comp1 has defeated comp2, ch belongs to comp1, and chlist is the
;;;        competitor history list recorded to that point
;;; RETURNS: the list of competitors that comp1 outranks
;;; DESIGN STRATEGY: Combine simpler functions

(define (insert-outranks-list comp1 comp2 ch chlist)
  (remove-dupes (cons comp2
                      (append (comp-history-outranks ch)
                              (loser-outranks comp2 chlist)))))

;;; TESTS
(begin-for-test
  (check-equal? (insert-outranks-list "A" "B"
                                 (make-comp-history "B" (list) (list "A"))
                                 empty)
                (list "B")
                "insert-outranks-list test 1 not correct"))

;;; loser-outranks : Competitor CompetitorHistoryList -> CompetitorList
;;; GIVEN: a competitor and a competitor history list
;;; WHERE: chlist is the competitor history list recorded to that point
;;; RETURNS: the list of competitors that the competitor has defeated
;;; DESIGN STRATEGY: If comp2 is on chlist use HOF filter, else return empty

(define (loser-outranks comp chlist)
  (if (competitor-on-list? comp chlist)
      (comp-history-outranks
       (first (filter
               ;; CompetitorHistory -> CompetitorHistory
               (lambda (x) (string=? comp (comp-history-name x)))
                      chlist)))
      empty))

;;; TESTS
(begin-for-test
  (check-equal? (loser-outranks "A" (list (make-comp-history "A" (list "B")
                                                             (list "A"))))
                (list "B")
                "loser-outranks test 1 not correct"))

;;; loser-outranked : Competitor CompetitorHistory -> CompetitorList
;;; GIVEN: a competitor and a competitor history
;;; RETURNS: the name of the competitor history if it outranks the competitor
;;; DESIGN STRATEGY: Cases on whether the competitor is outranked by the
;;;                  CompetitorHistory

(define (loser-outranked comp ch)
  (if (member? comp (comp-history-outranks ch))
      (list (comp-history-name ch))
      empty))

;;; TESTS
(begin-for-test
  (check-equal? (loser-outranked "A" (make-comp-history "A" (list "B")
                                                             (list "A")))
                (list)
                "loser-outranked test 1 not correct"))

;;; remove-dupes : StringList -> StringList
;;; GIVEN: a list of strings (any string will do)
;;; RETURNS: the given list with all duplicates removed
;;; EXAMPLE:
;;; (remove-dupes (list "c" "a" "b" "c")) => (list "a" "b" "c")
;;; DESIGN STRATEGY:  Use observer template for StringList

(define (remove-dupes list)
  (cond [(empty? list) empty]
        [(member? (first list) (rest list)) (remove-dupes (rest list))]
        [else (cons (first list) (remove-dupes (rest list)))]))

;;; TESTS:
(begin-for-test
  (check-equal? (remove-dupes (list "c" "a" "b" "c"))
                (list "a" "b" "c")
                "remove-dupes test 1 was not correct"))
