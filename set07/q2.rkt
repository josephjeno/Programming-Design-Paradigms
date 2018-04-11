;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "q1.rkt")

(check-location "07" "q2.rkt")

(provide lit
         literal-value
         var
         variable-name
         op
         operation-name
         call
         call-operator
         call-operands
         block
         block-var
         block-rhs
         block-body
         literal?
         variable?
         operation?
         call?
         block?
         undefined-variables
         well-typed?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BEGIN DATA DEFINITIONS

;;; An Int is one of:
;;;     -- a Literal
;;;     -- A Call WHERE the operator is type Op0 and all of its operands have
;;;        type Int
;;;     -- A Call WHERE the operator is type Op1, all of its operands have
;;;        type Int, and there is at least one operand

;;; OBSERVER TEMPLATE
;;; int-fn : ArithmeticExpression -> ??
#;
(define (int-fn exp)
  (cond [(literal? exp) ...]
        [(call? exp) (cond [(and (op0? (call-operator exp))
                                 (andmap int? (call-operands exp))) ...]
                           [(and (op1? (call-operator exp))
                                 (andmap int? (call-operands exp))
                                 (> (length (call-operands exp)) 0)) ...])]))


;;; An Op0 is one of:
;;;     -- an (op "+")
;;;     -- an (op "*")

;;; OBSERVER TEMPLATE
;;; op0-fn : Operation -> ??
#;
(define (op0-fn op)
  (...
   (operation-name op)))


;;; An Op1 is one of:
;;;     -- an (op "-")
;;;     -- an (op "/")

;;; OBSERVER TEMPLATE
;;; op1-fn : Operation -> ??
#;
(define (op1-fn op)
  (...
   (operation-name op)))


;;; An Error is one of:
;;;     -- a Variable WHERE the variable is used outside of any region
;;;     -- a Call WHERE the type is not Int
;;;     -- a Block WHERE the right-hand side has type error

;;; OBSERVER TEMPLATE
;;; error-fn : ArithmeticExpression -> ??
#;
(define (error-fn exp)
  (cond [(variable? exp) ...]
        [(call? exp) ...]
        [(block? exp) ...]))


;;; A type is one of
;;;     -- an Int
;;;     -- an Op0
;;;     -- an Op1
;;;     -- an Error

;;; OBSERVER TEMPLATE
;;; type-fn : ArithmeticExpression -> ??
#;
(define (type-fn exp)
  (cond [(int? exp) ...]
        [(op0? exp) ...]
        [(op1? exp) ...]
        [else ...]))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BEGIN FUNCTION DEFINITIONS

;;; well-typed? : ArithmeticExpression -> Boolean
;;; GIVEN: an arbitrary arithmetic expression
;;; RETURNS: true if and only if the expression is well-typed
;;; EXAMPLES:
;;;     (well-typed? (lit 17))  =>  true
;;;     (well-typed? (var "x"))  =>  false
;;;     (well-typed?
;;;      (block (var "f")
;;;             (op "+")
;;;             (block (var "x")
;;;                    (call (var "f") (list))
;;;                    (call (op "*")
;;;                          (list (var "x"))))) => true
;;;     (well-typed?
;;;      (block (var "f")
;;;             (op "+")
;;;             (block (var "f")
;;;                    (call (var "f") (list))
;;;                    (call (op "*")
;;;                          (list (var "f"))))) => true
;;;     (well-typed?
;;;      (block (var "f")
;;;             (op "+")
;;;             (block (var "x")
;;;                    (call (var "f") (list))
;;;                    (call (op "*")
;;;                          (list (var "f"))))) => false
;;; DESIGN STRATEGY:  Use observer template for arithmetic expression

(define (well-typed? exp)
  (cond [(literal? exp) true]
        [(variable? exp) false]
        [(operation? exp) true]
        [(call? exp) (well-typed-call? exp)]
        [(block? exp) (well-typed-block? exp)]))

;;; TESTS:
(begin-for-test
  (check-equal? (well-typed? (lit 17))
                true
                "well-typed? test 1 did not calculate correctly")
  (check-equal? (well-typed? (var "x"))
                false
                "well-typed? test 2 did not calculate correctly")
  (check-equal? (well-typed? (block (var "f")
                                    (op "+")
                                    (block (var "x")
                                           (call (var "f") (list))
                                           (call (op "*")
                                                 (list (var "x"))))))
                true
                "well-typed? test 3 did not calculate correctly")
  (check-equal? (well-typed? (block (var "f")
                                    (op "+")
                                    (block (var "f")
                                           (call (var "f") (list))
                                           (call (op "*")
                                                 (list (var "f"))))))
                true
                "well-typed? test 4 did not calculate correctly")
  (check-equal? (well-typed? (block (var "f")
                                    (op "+")
                                    (block (var "x")
                                           (call (var "f") (list))
                                           (call (op "*")
                                                 (list (var "f"))))))
                false
                "well-typed? test 4 did not calculate correctly"))


;;; well-typed-call? : Call -> Boolean
;;; GIVEN: an arbitrary Call
;;; RETURNS: true if and only if the Call is well-typed
;;; EXAMPLES:
;;; (well-typed-call? (call (op "+") empty)) => true
;;; (well-typed-call? (call (op "-") empty)) => false
;;; (well-typed-call? (call (op "*") (list (var "f")))) = false
;;; (well-typed-call? (call (op "/") (list (lit 17)))) = true
;;; DESIGN STRATEGY: Use observer template for Int

(define (well-typed-call? c)
  (cond [(and (op0? (call-operator c))
              (andmap int? (call-operands c))) true]
        [(and (op1? (call-operator c))
              (andmap int? (call-operands c))
              (> (length (call-operands c)) 0)) true]
        [else false]))

;;; TESTS:
(begin-for-test
  (check-equal? (well-typed-call? (call (op "+") empty))
                true
                "well-typed-call? test 1 did not calculate correctly")
  (check-equal? (well-typed-call? (call (op "-") empty))
                false
                "well-typed-call? test 2 did not calculate correctly")
  (check-equal? (well-typed-call? (call (op "*") (list (var "f"))))
                false
                "well-typed-call? test 3 did not calculate correctly")
  (check-equal? (well-typed-call? (call (op "/") (list (lit 17))))
                true
                "well-typed-call? test 4 did not calculate correctly"))


;;; well-typed-block? : Block -> Boolean
;;; GIVEN: an arbitrary Block
;;; RETURNS: true if and only if the Block is well-typed
;;; EXAMPLES:
;;; (well-typed-block? (block (var "f")
;;;                           (op "+")
;;;                           (block (var "x")
;;;                                  (call (var "f") (list))
;;;                                  (call (op "*")
;;;                                        (list (var "x")))))) => true
;;; (well-typed-block? (block (var "x")
;;;                           (call (var "f") (list))
;;;                           (call (op "*")
;;;                                 (list (var "f"))))) => false
;;; DESIGN STRATEGY : Cases on if the block-rhs is well-typed.

(define (well-typed-block? b)
  (if (well-typed? (block-rhs b))
      (well-typed? (var-update (block-body b) (block-var b) (block-rhs b)))
      false))

;;; TESTS:
(begin-for-test
  (check-equal? (well-typed-block? (block (var "f")
                                          (op "+")
                                          (block (var "x")
                                                 (call (var "f") (list))
                                                 (call (op "*")
                                                       (list (var "x"))))))
                true
                "well-typed-block? test 1 did not calculate correctly")
  (check-equal? (well-typed-block? (block (var "x")
                                          (call (var "f") (list))
                                          (call (op "*")
                                                (list (var "f")))))
                false
                "well-typed-block? test 2 did not calculate correctly"))


;;; int? : ArithmeticExpression -> Boolean
;;; GIVEN: an arbitrary arithmetic expression
;;; RETURNS: true if and only if the expression is of type Int
;;; EXAMPLES:
;;; (int? (lit 17)) => true
;;; (int? (call (op "/") (list (lit 17)))) => true
;;; (int? (var "x")) => false
;;; DESIGN STRATEGY: Use observer template for Int

(define (int? exp)
  (cond [(literal? exp) true]
        [(call? exp) (well-typed-call? exp)]
        [else false]))

;;; TESTS:
(begin-for-test
  (check-equal? (int? (lit 17))
                true
                "int? test 1 did not calculate correctly")
  (check-equal? (int? (call (op "/") (list (lit 17))))
                true
                "int? test 2 did not calculate correctly")
  (check-equal? (int? (var "x"))
                false
                "int? test 3 did not calculate correctly"))


;;; op0? : ArithmeticExpression -> Boolean
;;; GIVEN: an arbitrary arithmetic expression
;;; RETURNS: true if and only if the expression is of type Op0
;;; EXAMPLES:
;;; (op0? (op "+")) => true
;;; (op0? (lit 17)) => false
;;; DESIGN STRATEGY : Use observer template for Op0.

(define (op0? exp)
  (if (operation? exp)
      (or (string=? "+" (operation-name exp))
          (string=? "*" (operation-name exp)))
      false))

;;; TESTS:
(begin-for-test
  (check-equal? (op0? (op "+"))
                true
                "op0? test 1 did not calculate correctly")
  (check-equal? (op0? (lit 17))
                false
                "op0? test 2 did not calculate correctly"))


;;; op1? : ArithmeticExpression -> Boolean
;;; GIVEN: an arbitrary arithmetic expression
;;; RETURNS: true if and only if the expression is of type Op1
;;; EXAMPLES:
;;; (op1? (op "-")) => true
;;; (op1? (lit 17)) => false
;;; DESIGN STRATEGY : Use observer template for Op1.

(define (op1? exp)
  (if (operation? exp)
      (or (string=? "-" (operation-name exp))
          (string=? "/" (operation-name exp)))
      false))

;;; TESTS:
(begin-for-test
  (check-equal? (op1? (op "-"))
                true
                "op1? test 1 did not calculate correctly")
  (check-equal? (op1? (lit 17))
                false
                "op1? test 2 did not calculate correctly"))


;;; var-update :
;;;   ArithmeticExpression Variable ArithmeticExpression -> ArithmeticExpression
;;;   GIVEN: an arbitrary arithmetic expression exp, a variable var, and the
;;;          arithmetic expression rhs whose value is the value of the variable
;;; RETURNS: the arithmetic expression with any instances of var updated to 
;;;          be the value of the arithmetic expression rhs
;;; EXAMPLES:
;;; (var-update (var "f") (var "f") (op "+")) => (op "+")
;;; (var-update (var "x") (var "f") (op "+")) => (var "x")
;;; (var-update (call (op "*") (list (var "f"))) (var "f") (op "+"))
;;;                                           => (call (op "*") (list (op "+")))
;;; (var-update (block (var "x")
;;;                    (call (var "f") (list))
;;;                    (call (op "*")
;;;                          (list (var "x")))) (var "f") (op "+"))
;;;         => (block (var "x")
;;;                       (call (op "+") (list))
;;;                       (call (op "*") (list (var "x"))))
;;; DESIGN STRATEGY : Cases on Arithmetic Expression

(define (var-update exp var rhs)
  (cond [(variable? exp) (var-exp-injector exp var rhs)]
        [(call? exp) (var-update-call exp var rhs)]
        [(block? exp) (var-update-block exp var rhs)]
        [else exp]))

;;; TESTS:
(begin-for-test
  (check-equal? (var-update (var "f") (var "f") (op "+"))
                (op "+")
                "var-update test 1 did not calculate correctly")
  (check-equal? (var-update (var "x") (var "f") (op "+"))
                (var "x")
                "var-update test 2 did not calculate correctly")
  (check-equal? (var-update (call (op "*") (list (var "f"))) (var "f") (op "+"))
                (call (op "*") (list (op "+")))
                "var-update test 3 did not calculate correctly")
  (check-equal? (var-update (block (var "x")
                                   (call (var "f") (list))
                                   (call (op "*")
                                         (list (var "x")))) (var "f") (op "+"))
                (block (var "x")
                       (call (op "+") (list))
                       (call (op "*") (list (var "x"))))
                "var-update test 4 did not calculate correctly"))


;;; var-update-call : Call Variable ArithmeticExpression -> ArithmeticExpression
;;;   GIVEN: an arbitrary call c, a variable var, and the arithmetic expression
;;;          rhs whose value is the value of the variable
;;; RETURNS: the call with any instances of var updated to be the value of the 
;;;          arithmetic expression rhs
;;; EXAMPLE:
;;; (var-update-call (call (op "*") (list (var "f"))) (var "f") (op "+"))
;;;                                           => (call (op "*") (list (op "+")))
;;; DESIGN STRATEGY : Use constructor template for Call

(define (var-update-call c var rhs)
  (call (var-update-call-operator (call-operator c) var rhs)
        (var-update-call-operands (call-operands c) var rhs)))

;;; TESTS:
(begin-for-test
  (check-equal? (var-update-call (call (op "*") (list (var "f")))
                                 (var "f") (op "+"))
                (call (op "*") (list (op "+")))
                "var-update-call test 1 did not calculate correctly"))


;;; var-update-call-operator :  ArithmeticExpression Variable
;;;                             ArithmeticExpression -> ArithmeticExpression
;;;   GIVEN: an arithmetic expression exp, a variable var, and the arithmetic
;;;          expression rhs whose value is the value of the variable
;;; RETURNS: the arithmetic expression exp set to the arithmetic expression rhs
;;;          if exp is equal to variable var
;;; EXAMPLES:
;;; (var-update-call-operator (var "f") (var "f") (op "+")) => (op "+")
;;; (var-update-call-operator (lit 17) (var "f") (op "+")) => (lit 17)
;;; DESIGN STRATEGY : Cases on whether arithmetic expression is variable

(define (var-update-call-operator exp var rhs)
  (if (variable? exp) (var-exp-injector exp var rhs) exp))

;;; TESTS:
(begin-for-test
  (check-equal? (var-update-call-operator (var "f") (var "f") (op "+"))
                (op "+")
                "var-update-call-operator test 1 did not calculate correctly")
  (check-equal? (var-update-call-operator (lit 17) (var "f") (op "+"))
                (lit 17)
                "var-update-call-operator test 2 did not calculate correctly"))


;;; var-update-call-operands : ArithmeticExpressionList Variable
;;;                             ArithmeticExpression -> ArithmeticExpressionList
;;;   GIVEN: an arithmetic expression list explist, a variable var, and the 
;;;          arithmetic expression rhs whose value is the value of the variable
;;; RETURNS: the arithmetic expression list with any instances of var updated 
;;;          to be the value of the arithmetic expression rhs
;;; EXAMPLES:
;;; (var-update-call-operands (list (var "f")) (var "f") (op "+"))
;;;                                                           => (list (op "+"))
;;; (var-update-call-operands (list) (var "f") (op "+")) => (list)
;;; DESIGN STRATEGY : Use HOF map

(define (var-update-call-operands explist var rhs)
  (map (lambda (x) (var-update x var rhs)) explist))

;;; TESTS:
(begin-for-test
  (check-equal? (var-update-call-operands (list (var "f")) (var "f") (op "+"))
                (list (op "+"))
                "var-update-call-operands test 1 did not calculate correctly")
  (check-equal? (var-update-call-operands (list) (var "f") (op "+"))
                (list)
                "var-update-call-operands test 2 did not calculate correctly"))


;;; var-update-block :
;;;                  Block Variable ArithmeticExpression -> ArithmeticExpression
;;;   GIVEN: an arbitrary block b, a variable var, and the arithmetic expression
;;;          rhs whose value is the value of the variable
;;; RETURNS: the block with any instances of var updated to be the value of the 
;;;          arithmetic expression rhs
;;; EXAMPLE:
;;; (var-update-block (block (var "x")
;;;                          (call (var "f") (list))
;;;                          (call (op "*")
;;;                                (list (var "x")))) (var "f") (op "+"))
;;;             => (block (var "x")
;;;                       (call (op "+") (list))
;;;                       (call (op "*") (list (var "x"))))
;;; DESIGN STRATEGY : Use constructor template for block with cases on
;;;                   variable name to determine block-body

(define (var-update-block b var rhs)
  (block (block-var b)
         (var-update (block-rhs b) var rhs)
         (if (string=? (variable-name (block-var b)) (variable-name var))
             (block-body b)
             (var-update (block-body b) var rhs))))

;;; TESTS:
(begin-for-test
  (check-equal? (var-update-block (block (var "x")
                                   (call (var "f") (list))
                                   (call (op "*")
                                         (list (var "x")))) (var "f") (op "+"))
                (block (var "x")
                       (call (op "+") (list))
                       (call (op "*") (list (var "x"))))
                "var-update-block test 1 did not calculate correctly"))


;;; var-exp-injector :
;;;               Variable Variable ArithmeticExpression -> ArithmeticExpression
;;;   GIVEN: an arbitrary variable var1, a variable var2, and the arithmetic
;;;          expression rhs whose value is the value of var2
;;; RETURNS: the arithmetic expression rhs if var1 == var2, otherwise var1
;;; EXAMPLES:
;;; (var-exp-injector (var "f") (var "f") (op "+")) => (op "+")
;;; (var-exp-injector (var "x") (var "f") (op "+")) => (var "x")
;;; DESIGN STRATEGY : Cases on variable name

(define (var-exp-injector var1 var2 rhs)
  (if (string=? (variable-name var1) (variable-name var2)) rhs var1))

;;; TESTS:
(begin-for-test
  (check-equal? (var-exp-injector (var "f") (var "f") (op "+"))
                (op "+")
                "var-exp-injector test 1 did not calculate correctly")
  (check-equal? (var-exp-injector (var "x") (var "f") (op "+"))
                (var "x")
                "var-exp-injector test 2 did not calculate correctly"))