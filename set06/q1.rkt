;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(check-location "06" "q1.rkt")

(provide inner-product
         permutation-of?
         shortlex-less-than?
         permutations)

;;; inner-product : RealList RealList -> Real
;;; GIVEN: two lists of real numbers
;;; WHERE: the two lists have the same length
;;; RETURNS: the inner product of those lists
;;; EXAMPLES:
;;;     (inner-product (list 2.5) (list 3.0))  =>  7.5
;;;     (inner-product (list 1 2 3 4) (list 5 6 7 8))  =>  70
;;;     (inner-product (list) (list))  =>  0
;;; DESIGN STRATEGY: Use HOF map on list-x and list-y then use HOF foldr
;;;                  on resulting list

(define (inner-product list-x list-y)
  (foldr + 0 (map (lambda (x y) (* x y)) list-x list-y)))

;;; (define (inner-product list-x list-y)
;;;   (cond
;;;     [(empty? list-x) 0]
;;;     [else (+ (* (first list-x) (first list-y))
;;;              (inner-product (rest list-x) (rest list-y)))]))

;;; TESTS:
(begin-for-test
  (check-equal? (inner-product (list 2.5) (list 3.0)) 7.5
                "inner-product test 1 expected 7.5 as result")
  (check-equal? (inner-product (list 1 2 3 4) (list 5 6 7 8)) 70
                "inner-product test 2 expected 70 as result")
  (check-equal? (inner-product (list) (list)) 0
                "inner-product test 3 expected 0 as result"))


;;; permutation-of? : IntList IntList -> Boolean
;;; GIVEN: two lists of integers
;;; WHERE: neither list contains duplicate elements
;;; RETURNS: true if and only if one of the lists
;;;     is a permutation of the other
;;; EXAMPLES:
;;;     (permutation-of? (list 1 2 3) (list 1 2 3)) => true
;;;     (permutation-of? (list 3 1 2) (list 1 2 3)) => true
;;;     (permutation-of? (list 3 1 2) (list 1 2 4)) => false
;;;     (permutation-of? (list 1 2 3) (list 1 2)) => false
;;;     (permutation-of? (list) (list)) => true
;;; DESIGN STRATEGY: Cases on length of lists then use HOF andmap
;;;                  on sorted lists

(define (permutation-of? list-x list-y)
  (if (= (length list-x) (length list-y))
      (andmap = (sort list-x <) (sort list-y <))
      false))

;;; (define (permutation-of? list-x list-y)
;;;   (if (= (length list-x) (length list-y))
;;;       (permutation-of-equal-list? list-x list-y)
;;;       false))

;;; TESTS:
(begin-for-test
  (check-equal? (permutation-of? (list 1 2 3) (list 1 2 3)) true
                "permutation-of? test 1 expected true as result")
  (check-equal? (permutation-of? (list 3 1 2) (list 1 2 3)) true
                "permutation-of? test 2 expected true as result")
  (check-equal? (permutation-of? (list 3 1 2) (list 1 2 4)) false
                "permutation-of? test 3 expected false as result")
  (check-equal? (permutation-of? (list 1 2 3) (list 1 2)) false
                "permutation-of? test 4 expected false as result")
  (check-equal? (permutation-of? (list) (list)) true
                "permutation-of? test 5 expected true as result"))


;;; permutation-of-equal-list? : IntList IntList -> Boolean
;;; GIVEN: two lists of integers
;;; WHERE: neither list contains duplicate elements and both lists are the
;;;        same length
;;; RETURNS: true if and only if one of the lists
;;;     is a permutation of the other
;;; EXAMPLES:
;;;     (permutation-of-equal-list? (list 1 2 3) (list 1 2 3)) => true
;;;     (permutation-of-equal-list? (list 3 1 2) (list 1 2 3)) => true
;;;     (permutation-of-equal-list? (list 3 1 2) (list 1 2 4)) => false
;;;     (permutation-of-equal-list? (list) (list)) => true
;;; DESIGN STRATEGY: Cases on whether list is empty then boolean on int member
;;;                  of both lists
;;;
;;;
;;; (define (permutation-of-equal-list? list-x list-y)
;;;   (cond
;;;     [(empty? list-x) true]
;;;     [else (and (member? (first list-x) list-y)
;;;                (permutation-of-equal-list? (rest list-x)
;;;                                            (remove (first list-x)
;;;                                                    list-y)))]))
;;;
;;; TESTS:
;;; (begin-for-test
;;;   (check-equal? (permutation-of-equal-list? (list 1 2 3) (list 1 2 3)) true
;;;                 "permutation-of-equal-list? test 1 expected true as result")
;;;   (check-equal? (permutation-of-equal-list? (list 3 1 2) (list 1 2 3)) true
;;;                 "permutation-of-equal-list? test 2 expected true as result")
;;;   (check-equal? (permutation-of-equal-list? (list 3 1 2) (list 1 2 4)) false
;;;                 "permutation-of-equal-list? test 3 expected false")
;;;   (check-equal? (permutation-of-equal-list? (list) (list)) true
;;;                 "permutation-of-equal-list? test 4 expected true"))


;;; shortlex-less-than? : IntList IntList -> Boolean
;;; GIVEN: two lists of integers
;;; RETURNS: true if and only either
;;;     the first list is shorter than the second
;;;  or both are non-empty, have the same length, and either
;;;         the first element of the first list is less than
;;;             the first element of the second list
;;;      or the first elements are equal, and the rest of
;;;             the first list is less than the rest of the
;;;             second list according to shortlex-less-than?
;;; EXAMPLES:
;;;     (shortlex-less-than? (list) (list)) => false
;;;     (shortlex-less-than? (list) (list 3)) => true
;;;     (shortlex-less-than? (list 3) (list)) => false
;;;     (shortlex-less-than? (list 3) (list 3)) => false
;;;     (shortlex-less-than? (list 3) (list 1 2)) => true
;;;     (shortlex-less-than? (list 3 0) (list 1 2)) => false
;;;     (shortlex-less-than? (list 0 3) (list 1 2)) => true
;;; DESIGN STRATEGY: Cases on list lengths


(define (shortlex-less-than? list-x list-y)
  (cond
    [(< (length list-x) (length list-y)) true]
    [(> (length list-x) (length list-y)) false]
    [(and (empty? list-x) (empty? list-y)) false]
    [else (shortlex-less-than-equal-length? list-x list-y)]))

;;; TESTS:
(begin-for-test
  (check-equal? (shortlex-less-than? (list) (list)) false
                "shortlex-less-than? test 1 expected false as result")
  (check-equal? (shortlex-less-than? (list) (list 3)) true
                "shortlex-less-than? test 2 expected true as result")
  (check-equal? (shortlex-less-than? (list 3) (list)) false
                "shortlex-less-than? test 3 expected false as result")
  (check-equal? (shortlex-less-than? (list 3) (list 3)) false
                "shortlex-less-than? test 4 expected false as result")
  (check-equal? (shortlex-less-than? (list 3) (list 1 2)) true
                "shortlex-less-than? test 5 expected true as result")
  (check-equal? (shortlex-less-than? (list 3 0) (list 1 2)) false
                "shortlex-less-than? test 6 expected false as result")
  (check-equal? (shortlex-less-than? (list 0 3) (list 1 2)) true
                "shortlex-less-than? test 7 expected true as result"))


;;; shortlex-less-than-equal-length? : IntList IntList -> Boolean
;;; GIVEN: two lists of integers
;;; WHERE: both lists are the same length
;;; RETURNS: true if and only either
;;;         the first element of the first list is less than
;;;             the first element of the second list
;;;      or the first elements are equal, and the rest of
;;;             the first list is less than the rest of the
;;;             second list according to shortlex-less-than?
;;; EXAMPLES:
;;;     (shortlex-less-than-equal-length? (list) (list)) => false
;;;     (shortlex-less-than-equal-length? (list 3) (list 3)) => false
;;;     (shortlex-less-than-equal-length? (list 3 0) (list 1 2)) => false
;;;     (shortlex-less-than-equal-length? (list 0 3) (list 1 2)) => true
;;; DESIGN STRATEGY: Cases on first element size comparisons

(define (shortlex-less-than-equal-length? list-x list-y)
  (cond
    [(empty? list-y) false]
    [(< (first list-x) (first list-y)) true]
    [(> (first list-x) (first list-y)) false]
    [else (shortlex-less-than-equal-length? (rest list-x) (rest list-y))]))

;;; TESTS:
(begin-for-test
  (check-equal? (shortlex-less-than-equal-length? (list) (list)) false
                "shortlex-less-than-equal-length? test 1 expected false")
  (check-equal? (shortlex-less-than-equal-length? (list 3) (list 3)) false
                "shortlex-less-than-equal-length? test 4 expected false")
  (check-equal? (shortlex-less-than-equal-length? (list 3 0) (list 1 2)) false
                "shortlex-less-than-equal-length? test 6 expected false")
  (check-equal? (shortlex-less-than-equal-length? (list 0 3) (list 1 2)) true
                "shortlex-less-than-equal-length? test 7 expected true"))

;;; permutations : IntList -> IntListList
;;; GIVEN: a list of integers
;;; WHERE: the list contains no duplicates
;;; RETURNS: a list of all permutations of that list,
;;;     in shortlex order
;;; EXAMPLES:
;;;     (permutations (list))  =>  (list (list))
;;;     (permutations (list 9))  =>  (list (list 9))
;;;     (permutations (list 3 1 2))
;;;         =>  (list (list 1 2 3)
;;;                   (list 1 3 2)
;;;                   (list 2 1 3)
;;;                   (list 2 3 1)
;;;                   (list 3 1 2)
;;;                   (list 3 2 1))
;;; DESIGN STRATEGY: Combine simpler functions

(define (permutations l)
  (permutations-sorted (sort l <)))

;;; (define (permutations l)
;;;   (cond
;;;     [(empty? l) (list l)]
;;;     [(= (length l) 1) (list l)]
;;;     [else (permutations-lists (mysort l)
;;;                               (length l)
;;;                               (number-of-permutations (- (length l) 1))
;;;                               (list))]))

;;; TESTS
(begin-for-test
  (check-equal? (permutations (list)) (list (list))
                "permutations test 1 not correct")
  (check-equal? (permutations (list 9)) (list (list 9))
                "permutations test 2 not correct")
  (check-equal? (permutations (list 1 2 3))
                (list (list 1 2 3)
                      (list 1 3 2)
                      (list 2 1 3)
                      (list 2 3 1)
                      (list 3 1 2)
                      (list 3 2 1))
                "permutations test 3 not correct")
  (check-equal? (permutations (list 2 1 3))
                (list (list 1 2 3)
                      (list 1 3 2)
                      (list 2 1 3)
                      (list 2 3 1)
                      (list 3 1 2)
                      (list 3 2 1))
                "permutations test 4 not correct"))

;;; permutations-sorted : Intlist -> IntListList
;;; GIVEN: a list of integers
;;; WHERE: the list contains no duplicates
;;; RETURNS: a list of all permutations of that list, in shortlex order
;;; EXAMPLES:
;;;     (permutations-sorted (list))  =>  (list (list))
;;;     (permutations-sorted (list 9))  =>  (list (list 9))
;;;     (permutations-sorted (list 3 1 2))
;;;         =>  (list (list 1 2 3)
;;;                   (list 1 3 2)
;;;                   (list 2 1 3)
;;;                   (list 2 3 1)
;;;                   (list 3 1 2)
;;;                   (list 3 2 1))
;;; DESIGN STRATEGY: Cases on length of given IntList then use HOF map on the
;;;                  list

(define (permutations-sorted l)
  (cond [(empty? l) (list l)]
        [(= (length l) 1) (list l)]
        [else (apply append (map (permutations-cycler l) l))]))

;;; TESTS
(begin-for-test
  (check-equal? (permutations-sorted (list)) (list (list))
                "permutations test 1 not correct")
  (check-equal? (permutations-sorted (list 9)) (list (list 9))
                "permutations test 2 not correct")
  (check-equal? (permutations-sorted (list 1 2 3))
                (list (list 1 2 3)
                      (list 1 3 2)
                      (list 2 1 3)
                      (list 2 3 1)
                      (list 3 1 2)
                      (list 3 2 1))
                "permutations test 3 not correct"))

;;; permutations-cycler : IntList -> IntListList
;;; GIVEN: a list of integers
;;; WHERE: the list contains no duplicates
;;; RETURNS: a list of permutations of that list, in shortlex order
;;; EXAMPLES:
;;;     (permutations (list 3 1 2))
;;;         =>  (list (list (list 1 2 3)
;;;                         (list 1 3 2))
;;;                   (list (list 2 1 3)
;;;                         (list 2 3 1))
;;;                   (list (list 3 1 2)
;;;                         (list 3 2 1)))
;;; DESIGN STRATEGY: Use HOF map on list with x removed

(define (permutations-cycler l)
  ;; Int -> IntListList
  ;; RETURNS: a list of x added to the permutations of l with x removed
  (lambda(x) (map (permutations-rebuilder x)
                  (permutations-sorted (remove x l)))))

;;; TESTS
(begin-for-test
  (check-equal? (map (permutations-cycler (list 1 2 3)) (list 1 2 3))
                (list (list (list 1 2 3)
                            (list 1 3 2))
                      (list (list 2 1 3)
                            (list 2 3 1))
                      (list (list 3 1 2)
                            (list 3 2 1)))
                "permutations-cycler test 1 not correct"))

;;; permutations-rebuilder : Int -> IntList
;;; GIVEN: an Int
;;; RETURNS: a list with Int followed by given list
;;; EXAMPLES:
;;; (map (permutations-rebuilder 1) (list (list 2 3) (list 3 2))) =>
;;;                                             (list (list 1 2 3) (list 1 3 2))
;;; DESIGN STRATEGY: Use lambda to cons value to list

(define (permutations-rebuilder i)
  ;; IntList -> IntList
  (lambda(x) (cons i x)))

;;; TESTS
(begin-for-test
  (check-equal? (map (permutations-rebuilder 1)
                     (list (list 2 3) (list 3 2)))
                (list (list 1 2 3) (list 1 3 2))
                "permutations-rebuilder test 1 not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; OLD HELPER FUNCTIONS FOR PERMUTATIONS

;;; insert : Integer SortedIntList -> SortedIntList
;;; GIVEN: An integer and a sorted sequence
;;; RETURNS: A new SortedIntList just like the original, but with the
;;; new integer inserted.
;;; EXAMPLES:
;;; (insert 3 empty) = (list 3)
;;; (insert 3 (list 5 6)) = (list 3 5 6)
;;; (insert 3 (list -1 1 5 6)) = (list -1 1 3 5 6)
;;; STRATEGY: Use observer template for SortedIntList
;;;
;;; (define (insert n seq)
;;;   (cond
;;;     [(empty? seq) (cons n empty)]
;;;     [(< n (first seq)) (cons n seq)]
;;;     [else (cons (first seq)
;;;                 (insert n (rest seq)))]))
;;;
;;; (begin-for-test
;;;   (check-equal? (insert 3 empty)  (list 3))
;;;   (check-equal? (insert 3 (list 5 6))  (list 3 5 6))
;;;   (check-equal? (insert 3 (list -1 1 5 6))  (list -1 1 3 5 6)))
;;;
;;;
;;; mysort : IntList -> SortedIntList
;;; GIVEN: An integer sequence
;;; RETURNS: The same sequence, only sorted by <= .
;;; EXAMPLES:
;;; (mysort empty) = empty
;;; (mysort (list 3)) = (list 3)
;;; (mysort (list 2 1 4)) = (list 1 2 4)
;;; (mysort (list 2 1 4 2)) = (list 1 2 2 4)
;;; STRATEGY: Use observer template for IntList
;;;
;;; (define (mysort ints)
;;;   (cond
;;;     [(empty? ints) empty]
;;;     [else (insert (first ints)
;;;                   (mysort (rest ints)))]))
;;;
;;; (begin-for-test
;;;   (check-equal? (mysort empty) empty)
;;;   (check-equal? (mysort (list 3)) (list 3))
;;;   (check-equal? (mysort (list 2 1 4)) (list 1 2 4))
;;;   (check-equal? (mysort (list 2 1 4 2)) (list 1 2 2 4)))
;;;
;;;
;;; number-of-permutations : Int -> Int
;;; GIVEN: An integer
;;; RETURNS: The factorial of that integer
;;; EXAMPLES:
;;;  (number-of-permutations 2) => 2
;;;  (number-of-permutations 3) => 6
;;;  (number-of-permutations 4) => 24
;;;
;;; (define (number-of-permutations n)
;;;   (cond
;;;     [(= n 1) 1]
;;;     [else (* n (number-of-permutations (- n 1)))]))
;;;
;;; (begin-for-test
;;;   (check-equal? (number-of-permutations 2) 2
;;;                 "number-of-permutations test 1 not correct")
;;;   (check-equal? (number-of-permutations 3) 6
;;;                 "number-of-permutations test 2 not correct")
;;;   (check-equal? (number-of-permutations 4) 24
;;;                 "number-of-permutations test 3 not correct"))
;;; permutations-lists : IntList Int Int IntListList -> IntListList
;;; GIVEN: an Intlist, the number of primary ints left to cycle, the number of
;;;        ints left to cycle for the current primary int, and the sorted
;;;        IntListList
;;; RETURNS: a list of all permutations of that Intlist,
;;;     in shortlex order
;;; EXAMPLES:
;;;     (permutations-lists (list 1 2))
;;;         =>  (list (list 1 2)
;;;                   (list 2 1))
;;; DESIGN STRATEGY: Cases on primary ints left to cycle
;;;
;;; (define (permutations-lists l i t ill)
;;;   (cond [(and (= i 1) (= t 0)) ill]
;;;         [else (permutations-group l i t ill)]))
;;;
;;; (begin-for-test
;;;   (check-equal? (permutations-lists (list 1 2) 2 1 empty)
;;;                 (list (list 1 2) (list 2 1))
;;;                 "permutations test 1 not correct"))
;;;
;;; permutations-group : IntList Int Int IntListList -> IntListList
;;; GIVEN: an Intlist, the number of primary ints left to cycle, the number of
;;;        ints left to cycle for the current primary int, and the sorted
;;;        IntListList
;;; RETURNS: a list of all permutations of that Intlist,
;;;     in shortlex order
;;; EXAMPLES:
;;;     (permutations-lists (list 1 2))
;;;         =>  (list (list 1 2)
;;;                   (list 2 1))
;;; DESIGN STRATEGY: Cases on ints left to cycle in current primary group
;;;
;;; (define (permutations-group l i t ill)
;;;   (if (= t 0)
;;;       (permutations-lists (permutations-iterate l i t ill)
;;;                           (- i 1)
;;;                           (number-of-permutations (- (length l) 1))
;;;                           ill)
;;;       (permutations-number (cons (first l)
;;;                                  (randomlist (remove (first l) l)
;;;                                              (random (- (length l) 1))))
;;;                            i
;;;                            t
;;;                            ill)))
;;;
;;; (begin-for-test
;;;   (check-equal? (permutations-group (list 1 2) 2 1 empty)
;;;                 (list (list 1 2) (list 2 1))
;;;                 "permutations-group test 1 not correct"))
;;;
;;; permutations-number : IntList Int Int IntListList -> IntListList
;;; GIVEN: an Intlist, the number of primary ints left to cycle, the number of
;;;        ints left to cycle for the current primary int, and the sorted
;;;        IntListList
;;; RETURNS: a list of all permutations of that Intlist,
;;;     in shortlex order
;;; EXAMPLES:
;;;     (permutations-lists (list 1 2))
;;;         =>  (list (list 1 2)
;;;                   (list 2 1))
;;; DESIGN STRATEGY: Cases on ints left to cycle in current primary group
;;;
;;; (define (permutations-number l i t ill)
;;;   (if (member? l ill)
;;;       (permutations-lists l i t ill)
;;;       (permutations-lists l i (- t 1) (permutations-add l ill))))
;;;
;;; (begin-for-test
;;;   (check-equal? (permutations-number (list 1 2) 2 1 empty)
;;;                 (list (list 1 2) (list 2 1))
;;;                 "permutations-number test 1 not correct")
;;;   (check-equal? (permutations-number (list 1) 1 0 (list (list 1)))
;;;                 (list (list 1))
;;;                 "permutations-number test 2 not correct"))
;;;
;;; permutations-add : IntList SortedIntList -> SortedIntList
;;; GIVEN: An IntList and a SortedIntList
;;; RETURNS: the SortedIntList with the IntList inserted
;;; EXAMPLES:
;;; See below testing
;;; DESIGN STRATEGY: Cases on length of SortedIntList
;;;
;;; (define (permutations-add l ill)
;;;   (cond [(= (length ill) 0) (list l)]
;;;         [(shortlex-less-than? l (first ill)) (cons l ill)]
;;;         [else (cons (first ill) (permutations-add l (rest ill)))]))
;;;
;;; (begin-for-test
;;;   (check-equal? (permutations-add (list 3 2) (list (list 1 2) (list 2 1)))
;;;                 (list (list 1 2) (list 2 1) (list 3 2))
;;;                 "permutations-add test 1 not correct")
;;;   (check-equal? (permutations-add (list 1 2) (list (list 1 3) (list 2 1)))
;;;                 (list (list 1 2) (list 1 3) (list 2 1))
;;;                 "permutations-add test 2 not correct"))
;;;
;;;
;;; permutations-iterate-primary : IntList Int Int IntListList -> IntListList
;;; GIVEN: an Intlist, the number of primary ints left to cycle, the number of
;;;        ints left to cycle for the current primary int, and the sorted
;;;        IntListList
;;; RETURNS: a list of all permutations of that Intlist,
;;;     in shortlex order
;;; EXAMPLES:
;;; See below testing 
;;; DESIGN STRATEGY: Combine simpler functions
;;;
;;; (define (permutations-iterate l i t ill)
;;;   (cons (list-ref l (- (length l) (- i 1)))
;;;         (mysort (remove (list-ref l (- (length l) (- i 1))) l))))
;;;
;;; (begin-for-test
;;;   (check-equal? (permutations-number (list 1 2) 2 1 empty)
;;;                 (list (list 1 2) (list 2 1))
;;;                 "permutations-group test 1 not correct"))
;;;
;;;
;;; randomlist : IntList Int -> IntList
;;; GIVEN: An IntList and an Int
;;; RETURNS: The IntList randomized
;;; EXAMPLES:
;;; See below testing
;;; DESIGN STRATEGY: Cases on length of IntList
;;;
;;; (define (randomlist l n)
;;;   (cond [(= (length l) 0) empty]
;;;         [(= (length l) 1) l]
;;;         [else (cons (list-ref l n)
;;;                (randomlist (remove (list-ref l n) l)
;;;                            (random (- (length l) 1))))]))
;;;
;;; (begin-for-test
;;;   (check-equal? (randomlist (list 1 2) 1)
;;;                 (list 2 1)
;;;                 "randomlist test 1 not correct")
;;;   (check-equal? (randomlist (list) 1)
;;;                 (list)
;;;                 "randomlist test 1 not correct"))
;;;