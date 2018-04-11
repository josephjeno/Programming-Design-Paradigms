;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(check-location "07" "q1.rkt")

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
         undefined-variables)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BEGIN DATA DEFINITIONS

;;; An OperationName is represented as one of the following strings:
;;;     -- "+"      (indicating addition)
;;;     -- "-"      (indicating subtraction)
;;;     -- "*"      (indicating multiplication)
;;;     -- "/"      (indicating division)
;;;
;;; OBSERVER TEMPLATE:
;;; operation-name-fn : OperationName -> ??
#;
(define (operation-name-fn op)
  (cond ((string=? op "+") ...)
        ((string=? op "-") ...)
        ((string=? op "*") ...)
        ((string=? op "/") ...)))


;;; An ArithmeticExpression is one of
;;;     -- a Literal
;;;     -- a Variable
;;;     -- an Operation
;;;     -- a Call
;;;     -- a Block
;;;
;;; OBSERVER TEMPLATE:
;;; arithmetic-expression-fn : ArithmeticExpression -> ??
#;
(define (arithmetic-expression-fn exp)
  (cond ((literal? exp) ...)
        ((variable? exp) ...)
        ((operation? exp) ...)
        ((call? exp) ...)
        ((block? exp) ...)))


;;; An ArithmeticExpressionList is represented as a list of ArithmeticExpression

;;; CONSTRUCTOR TEMPLATE AND INTERPRETATION
;;; empty                                      -- the empty sequence
;;; (cons exp explist)
;;;   WHERE:
;;;    exp is an ArithmeticExpression          -- the first exp in the sequence
;;;    explist is an ArithmeticExpressionList  -- the rest of the expressions in
;;;                                               the sequence

;;; OBSERVER TEMPLATE:
;;; explist-fn : ArithmeticExpressionList -> ??
#;
(define (explist-fn lst)
  (cond
    [(empty? lst) ...]
    [else (... (first lst)
               (explist-fn (rest lst)))]))


;;; An OperationExpression is one of
;;;      -- an Operation
;;;      -- a Block whose body is an OperationExpression
;;;
;;; OBSERVER TEMPLATE:
;;; operation-expression-fn : OperationExpression -> ??
#;
(define (operation-expression-fn exp)
  (cond ((operation? exp) ...)
        ((block? exp) ...)))


;;; A ConstantExpression is one of
;;;      -- a Literal
;;;      -- a Call whose operator is an OperationExpression and whose operands
;;;         are all ConstantExpressions
;;;      -- a Block whose body is a ConstantExpression
;;;
;;; OBSERVER TEMPLATE:
;;; constant-expression-fn : ConstantExpression -> ??
#;
(define (constant-expression-fn exp)
  (cond [(literal? exp) ...]
        [(call? exp) ...]
        [(block? exp) ...]))


;;; A Literal is represented as a (make-literal value)
;;; with the following fields:
;;; value : Real     represents the literal's value

;;; IMPLEMENTATION
(define-struct literal (value))

;;; CONSTRUCTOR TEMPLATE
;;; (make-literal Real)

;;; OBSERVER TEMPLATE
;;; literal-fn : Literal -> ??
#;
(define (literal-fn l)
  (...
   (literal-value l)))


;;; A Variable is represented as a (make-variable name)
;;; with the following fields:
;;; name : String     a string that begins with a letter and contains only
;;;                   numbers and letters

;;; IMPLEMENTATION
(define-struct variable (name))

;;; CONSTRUCTOR TEMPLATE
;;; (make-variable String)

;;; OBSERVER TEMPLATE
;;; variable-fn : Variable -> ??
#;
(define (variable-fn v)
  (...
   (variable-name v)))


;;; An Operation is represented as a (make-operation name)
;;; with the following fields:
;;; name : OperationName    represents an arithmetic operation such as addition
;;;                         or division

;;; IMPLEMENTATION
(define-struct operation (name))

;;; CONSTRUCTOR TEMPLATE
;;; (make-operation OperationName)

;;; OBSERVER TEMPLATE
;;; operation-fn : Operation -> ??
#;
(define (operation-fn o)
  (...
   (operation-name o)))


;;; A Call is represented as a (make-func-call operator operands)
;;; with the following fields:
;;; operator : ArithmeticExpression       the operator expression of the call
;;; operands : ArithmeticExpressionList   the operand expression of the call

;;; IMPLEMENTATION
(define-struct func-call (operator operands))

;;; CONSTRUCTOR TEMPLATE
;;; (make-func-call ArithmeticExpression ArithmeticExpressionList)

;;; OBSERVER TEMPLATE
;;; func-call-fn : Call -> ??
#;
(define (func-call-fn c)
  (...
   (func-call-operator c)
   (func-call-operands c)))


;;; A Block is represented as a (make-block-struct var rhs body)
;;; with the following fields:
;;;  var : Variable                the variable defined by the block
;;;  rhs : ArithmeticExpression    becomes the value of the defined variable
;;; body : ArithmeticExpression    becoems the value of the block expression

;;; IMPLEMENTATION
(define-struct block-struct (var rhs body))

;;; CONSTRUCTOR TEMPLATE
;;; (make-block-struct Variable ArithmeticExpression ArithmeticExpression)

;;; OBSERVER TEMPLATE
;;; block-struct-fn : Block -> ??
#;
(define (block-struct-fn b)
  (...
   (block-struct-var b)
   (block-struct-rhs b)
   (block-struct-body b)))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BEGIN FUNCTION DEFINITIONS

;;; lit : Real -> Literal
;;; GIVEN: a real number
;;; RETURNS: a literal that represents that number
;;; EXAMPLE: (see the example given for literal-value,
;;;          which shows the desired combined behavior
;;;          of lit and literal-value)
;;; DESIGN STRATEGY: Use constructor template

(define (lit n)
  (make-literal n))
          
;;; literal-value : Literal -> Real
;;; GIVEN: a literal
;;; RETURNS: the number it represents
;;; EXAMPLE: (literal-value (lit 17.4)) => 17.4

;;; Defined by data definitions

;;; TESTS:
(begin-for-test
  (check-equal? (literal-value (lit 17.4)) 17.4
                "literal-value test 1 did not calculate correctly"))
          
;;; var : String -> Variable
;;; GIVEN: a string
;;; WHERE: the string begins with a letter and contains
;;;     nothing but letters and digits
;;; RETURNS: a variable whose name is the given string
;;; EXAMPLE: (see the example given for variable-name,
;;;          which shows the desired combined behavior
;;;          of var and variable-name)
;;; DESIGN STRATEGY: Use constructor template

(define (var s)
  (make-variable s))
          
;;; variable-name : Variable -> String
;;; GIVEN: a variable
;;; RETURNS: the name of that variable
;;; EXAMPLE: (variable-name (var "x15")) => "x15"

;;; Defined by data definitions

;;; TESTS:
(begin-for-test
  (check-equal? (variable-name (var "x15")) "x15"
                "variable-name test 1 did not calculate correctly"))

;;; op : OperationName -> Operation
;;; GIVEN: the name of an operation
;;; RETURNS: the operation with that name
;;; EXAMPLES: (see the examples given for operation-name,
;;;           which show the desired combined behavior
;;;           of op and operation-name)
;;; DESIGN STRATEGY: Use constructor template

(define (op opname)
  (make-operation opname))

;;; operation-name : Operation -> OperationName
;;; GIVEN: an operation
;;; RETURNS: the name of that operation
;;; EXAMPLES:
;;;     (operation-name (op "+")) => "+"
;;;     (operation-name (op "/")) => "/"

;;; Defined by data definitions

;;; TESTS:
(begin-for-test
  (check-equal? (operation-name (op "+")) "+"
                "operation-name test 1 did not calculate correctly")
  (check-equal? (operation-name (op "/")) "/"
                "operation-name test 2 did not calculate correctly"))

;;; call : ArithmeticExpression ArithmeticExpressionList -> Call
;;; GIVEN: an operator expression and a list of operand expressions
;;; RETURNS: a call expression whose operator and operands are as
;;;     given
;;; EXAMPLES: (see the examples given for call-operator and
;;;           call-operands, which show the desired combined
;;;           behavior of call and those functions)
;;; DESIGN STRATEGY: Use constructor template

(define (call operator operands)
  (make-func-call operator operands))

;;; call-operator : Call -> ArithmeticExpression
;;; GIVEN: a call
;;; RETURNS: the operator expression of that call
;;; EXAMPLE:
;;;     (call-operator (call (op "-")
;;;                          (list (lit 7) (lit 2.5))))
;;;         => (op "-")
;;; DESIGN STRATEGY: Use observer template

(define (call-operator fcall)
  (func-call-operator fcall))

;;; TESTS:
(begin-for-test
  (check-equal? (call-operator (call (op "-") (list (lit 7) (lit 2.5))))
                (op "-")
                "call-operator test 1 did not calculate correctly"))
          
;;; call-operands : Call -> ArithmeticExpressionList
;;; GIVEN: a call
;;; RETURNS: the operand expressions of that call
;;; EXAMPLE:
;;;     (call-operands (call (op "-")
;;;                          (list (lit 7) (lit 2.5))))
;;;         => (list (lit 7) (lit 2.5))
;;; DESIGN STRATEGY: Use observer template

(define (call-operands fcall)
  (func-call-operands fcall))

;;; TESTS:
(begin-for-test
  (check-equal? (call-operands (call (op "-") (list (lit 7) (lit 2.5))))
                (list (lit 7) (lit 2.5))
                "call-operands test 1 did not calculate correctly"))
          
;;; block : Variable ArithmeticExpression ArithmeticExpression
;;;             -> Block
;;; GIVEN: a variable, an expression e0, and an expression e1
;;; RETURNS: a block that defines the variable's value as the
;;;     value of e0; the block's value will be the value of e1
;;; EXAMPLES: (see the examples given for block-var, block-rhs,
;;;           and block-body, which show the desired combined
;;;           behavior of block and those functions)
;;; DESIGN STRATEGY: Use constructor template

(define (block var e0 e1)
  (make-block-struct var e0 e1))
          
;;; block-var : Block -> Variable
;;; GIVEN: a block
;;; RETURNS: the variable defined by that block
;;; EXAMPLE:
;;;     (block-var (block (var "x5")
;;;                       (lit 5)
;;;                       (call (op "*")
;;;                             (list (var "x6") (var "x7")))))
;;;         => (var "x5")
;;; DESIGN STRATEGY: Use observer template

(define (block-var b)
  (block-struct-var b))

;;; TESTS:
(begin-for-test
  (check-equal? (block-var (block (var "x5")
                                  (lit 5)
                                  (call (op "*") (list (var "x6") (var "x7")))))
                (var "x5")
                "block-var test 1 did not calculate correctly"))
          
;;; block-rhs : Block -> ArithmeticExpression
;;; GIVEN: a block
;;; RETURNS: the expression whose value will become the value of
;;;     the variable defined by that block
;;; EXAMPLE:
;;;     (block-rhs (block (var "x5")
;;;                       (lit 5)
;;;                       (call (op "*")
;;;                             (list (var "x6") (var "x7")))))
;;;         => (lit 5)
;;; DESIGN STRATEGY: Use observer template

(define (block-rhs b)
  (block-struct-rhs b))

;;; TESTS:
(begin-for-test
  (check-equal? (block-rhs (block (var "x5")
                                  (lit 5)
                                  (call (op "*") (list (var "x6") (var "x7")))))
                (lit 5)
                "block-rhs test 1 did not calculate correctly"))
          
;;; block-body : Block -> ArithmeticExpression
;;; GIVEN: a block
;;; RETURNS: the expression whose value will become the value of
;;;     the block expression
;;; EXAMPLE:
;;;     (block-body (block (var "x5")
;;;                        (lit 5)
;;;                        (call (op "*")
;;;                              (list (var "x6") (var "x7")))))
;;;         => (call (op "*") (list (var "x6") (var "x7")))
;;; DESIGN STRATEGY: Use observer template

(define (block-body b)
  (block-struct-body b))

;;; TESTS:
(begin-for-test
  (check-equal? (block-body (block (var "x5")
                                  (lit 5)
                                  (call (op "*") (list (var "x6") (var "x7")))))
                (call (op "*") (list (var "x6") (var "x7")))
                "block-body test 1 did not calculate correctly"))
          
;;; literal?   : ArithmeticExpression -> Boolean
;;; variable?  : ArithmeticExpression -> Boolean
;;; operation? : ArithmeticExpression -> Boolean
;;; call?      : ArithmeticExpression -> Boolean
;;; block?     : ArithmeticExpression -> Boolean
;;; GIVEN: an arithmetic expression
;;; RETURNS: true if and only the expression is (respectively)
;;;     a literal, variable, operation, call, or block
;;; EXAMPLES:
;;;     (variable? (block-body (block (var "y") (lit 3) (var "z"))))
;;;         => true
;;;     (variable? (block-rhs (block (var "y") (lit 3) (var "z"))))
;;;         => false
;;; DESIGN STRATEGY: Use constructor template

;;; literal? - Defined by data definitions
;;; variable? - Defined by data definitions
;;; operation? - Defined by data definitions

(define (call? c)
  (func-call? c))

(define (block? b)
  (block-struct? b))

;;; TESTS:
(begin-for-test
  (check-equal? (variable? (block-body (block (var "y") (lit 3) (var "z"))))
                true
                "variable? test 1 did not calculate correctly")
  (check-equal? (variable? (block-rhs (block (var "y") (lit 3) (var "z"))))
                false
                "variable? test 2 did not calculate correctly")
  (check-equal? (call? (call (op "-") (list (lit 7) (lit 2.5))))
                true
                "variable? test 3 did not calculate correctly")
  (check-equal? (block? (block (var "x5")
                               (lit 5)
                               (call (op "*") (list (var "x6") (var "x7")))))
                true
                "variable? test 4 did not calculate correctly"))


;;; undefined-variables : ArithmeticExpression -> StringList
;;; GIVEN: an arbitrary arithmetic expression
;;; RETURNS: a list of the names of all undefined variables
;;;     for the expression, without repetitions, in any order
;;; EXAMPLE:
;;;     (undefined-variables
;;;      (call (var "f")
;;;            (list (block (var "x")
;;;                         (var "x")
;;;                         (var "x"))
;;;                  (block (var "y")
;;;                         (lit 7)
;;;                         (var "y"))
;;;                  (var "z"))))
;;;  => some permutation of (list "f" "x" "z")
;;; DESIGN STRATEGY: Cases on type of ArithmeticExpression

(define (undefined-variables exp)
  (remove-dupes (cond [(variable? exp) (list (variable-name exp))]
                      [(call? exp) (undefined-variables-call exp)]
                      [(block? exp) (undefined-variables-block exp)]
                      [else empty])))

;;; TESTS:
(begin-for-test
  (check-equal? (undefined-variables (call (var "f")
                                           (list (block (var "x")
                                                        (var "x")
                                                        (var "x"))
                                                 (block (var "y")
                                                        (lit 7)
                                                        (var "y"))
                                                 (var "z"))))
                (list "f" "x" "z")
                "undefined-variables test 1 did not calculate correctly")
  (check-equal? (undefined-variables (call (var "f")
                           (list (call (var "g")
                                       empty)
                                 (var "i")
                                 (var "i"))))
                (list "f" "g" "i")
                "undefined-variables test 2 did not calculate correctly")
  (check-equal? (undefined-variables (var "f"))
                (list "f")
                "undefined-variables test 3 did not calculate correctly"))

;;; undefined-variables-call : Call -> StringList
;;; GIVEN: an arbitrary Call
;;; RETURNS: a list of the names of all undefined variables
;;;     for the Call, in any order
;;; EXAMPLE:
;;;     (undefined-variables-call
;;;      (call (var "f")
;;;            (list (block (var "x")
;;;                         (var "x")
;;;                         (var "x"))
;;;                  (block (var "y")
;;;                         (lit 7)
;;;                         (var "y"))
;;;                  (var "z"))))
;;;  => (list "f" "x" "z")
;;; DESIGN STRATEGY: Observer template for Call using HOF foldr on Call operands

(define (undefined-variables-call exp)
  (append (undefined-variables (call-operator exp))
          (foldr (lambda (x r) (append (undefined-variables x) r))
                 empty
                 (call-operands exp))))

;;; TESTS:
(begin-for-test
  (check-equal? (undefined-variables-call (call (var "f")
                                                (list (block (var "x")
                                                             (var "x")
                                                             (var "x"))
                                                      (block (var "y")
                                                             (lit 7)
                                                             (var "y"))
                                                      (var "z"))))
                (list "f" "x" "z")
                "undefined-variables-call test 1 did not calculate correctly")
  (check-equal? (undefined-variables-call (call (var "f")
                                                (list (call (var "g")
                                                            empty)
                                                      (var "i"))))
                (list "f" "g" "i")
                "undefined-variables-call test 2 did not calculate correctly"))

;;; undefined-variables-block : Block -> StringList
;;; GIVEN: an arbitrary Block
;;; RETURNS: a list of the names of all undefined variables
;;;     for the Block, in any order
;;; EXAMPLE:
;;;     (undefined-variables-block
;;;                  (block (var "x")
;;;                         (var "x")
;;;                         (var "x"))
;;;  => (list "x")
;;; DESIGN STRATEGY: Use observer template for Block

(define (undefined-variables-block exp)
  (append (undefined-variables (block-rhs exp))
          (if (variable? (block-body exp))
              empty
              (undefined-variables (block-body exp)))))

;;; TESTS:
(begin-for-test
  (check-equal? (undefined-variables-block (block (var "x")
                                                  (var "x")
                                                  (var "x")))
                (list "x")
                "undefined-variables test 1 did not calculate correctly")
  (check-equal? (undefined-variables-block (block (var "y")
                                                  (lit 7)
                                                  (call (var "y")
                                                        empty)))
                (list "y")
                "undefined-variables test 2 did not calculate correctly"))

;;; remove-dupes : StringList -> StringList
;;; GIVEN: a list of strings (any string will do)
;;; RETURNS: the given list with all duplicates removed
;;; EXAMPLE:
;;; (remove-dupes (list "c" "a" "b" "c")) => (list "a" "b" "c")
;;; DESIGN STRATEGY:  Use HOF foldr

(define (remove-dupes list)
  (foldr
   ;; String StringList -> StringList
   ;; RETURNS: the StringList with duplicates removed
   (lambda (x r) (if (member? x r) r (cons x r))) empty list))

;;; TESTS:
(begin-for-test
  (check-equal? (remove-dupes (list "c" "a" "b" "c"))
                (list "a" "b" "c")
                "remove-dupes test 1 did not calculate correctly"))

