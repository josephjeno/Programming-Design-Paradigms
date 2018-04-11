;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(check-location "05" "q2.rkt")

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
         variables-defined-by
         variables-used-by
         constant-expression?
         constant-expression-value)

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
;;;             -> ArithmeticExpression
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


;;; variables-defined-by : ArithmeticExpression -> StringList
;;; GIVEN: an arithmetic expression
;;; RETURNS: a list of the names of all variables defined by
;;;     all blocks that occur within the expression, without
;;;     repetitions, in any order
;;; EXAMPLE:
;;;     (variables-defined-by
;;;      (block (var "x")
;;;             (var "y")
;;;             (call (block (var "z")
;;;                          (var "x")
;;;                          (op "+"))
;;;                   (list (block (var "x")
;;;                                (lit 5)
;;;                                (var "x"))
;;;                         (var "x")))))
;;;  => (list "x" "z") or (list "z" "x")
;;; DESIGN STRATEGY: Cases on type of arithmetic expression

(define (variables-defined-by exp)
  (remove-dupes (cond [(block? exp) (variables-defined-by-block exp)]
                      [(call? exp) (variables-defined-by-call exp)]
                      [else (list)])))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-defined-by
                 (block (var "x")
                        (var "y")
                        (call (block (var "z")
                                     (var "x")
                                     (op "+"))
                              (list (block (var "x")
                                           (lit 5)
                                           (var "x"))
                                    (var "x")))))
                (list "z" "x")
                "variables-defined-by test 1 did not calculate correctly")
  (check-equal? (variables-defined-by (call (op "+") empty)) (list)
                "variables-defined-by test 2 did not calculate correctly"))


;;; variables-defined-by-block : Block -> StringList
;;; GIVEN: a Block
;;; RETURNS: a list of all variables defined by the block and all blocks that
;;;     occur within the expression, without repitition, in any order
;;; EXAMPLE:
;;; (variables-defined-by-block (block (var "x") (lit 5) (var "z"))) =>
;;;                                                     (list "x")
;;; DESIGN STRATEGY: Use observer template for Block

(define (variables-defined-by-block exp)
  (append (list (variable-name (block-var exp)))
          (variables-defined-by (block-rhs exp))
          (variables-defined-by (block-body exp))))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-defined-by-block (block (var "x")
                                                   (lit 5)
                                                   (var "z")))
                (list "x")
                "variables-defined-by-block test 1 was not correct"))


;;; variables-defined-by-call : Call -> StringList
;;; GIVEN: a Call
;;; RETURNS: a list of all variables defined by all blocks within the call,
;;;          without repitition, in any order
;;; EXAMPLE:
;;; (variables-defined-by-call (call (block (var "z") (var "x") (op "+"))
;;;                                   (list (block (var "x")
;;;                                                (lit 5)
;;;                                                (var "x"))
;;;                                         (var "x"))     
;;;                                 => (list "z" "x")
;;; DESIGN STRATEGY: Use observer template for Call

(define (variables-defined-by-call exp)
  (append (variables-defined-by (call-operator exp))
          (variables-defined-by-call-ops (call-operands exp))))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-defined-by-call (call (block (var "z")
                                                         (var "x")
                                                         (op "+"))
                                                  (list (block (var "x")
                                                               (lit 5)
                                                               (var "x"))
                                                        (var "x"))))
                (list "z" "x")
                "variables-defined-by-call test 1 was not correct"))


;;; variables-defined-by-call-ops : ArithmeticExpressionList -> StringList
;;; GIVEN: a list of arithmetic expressions
;;; RETURNS: a list of all variables defined by all blocks wihin the list,
;;;          without repitition, in any order
;;; EXAMPLE:
;;; (variables-defined-by-call-ops (list (block (var "x") (lit 5) (var "x"))
;;;                                      (var "z")))  => (list "z" "x")
;;; DESIGN STRATEGY: Use Observer Template for ArithmeticExpressionList

(define (variables-defined-by-call-ops explist)
  (cond [(empty? explist) (list)]
        [else (append (variables-defined-by (first explist))
                      (variables-defined-by-call-ops (rest explist)))]))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-defined-by-call-ops (list (block (var "x")
                                                            (lit 5)
                                                            (var "x"))
                                                     (var "z")))
                (list "x")
                "variables-defined-by-call-ops test 1 was not correct"))


;;; remove-dupes : StringList -> StringList
;;; GIVEN: a list of strings (any string will do)
;;; RETURNS: the given list with all duplicates removed
;;; EXAMPLE:
;;; (remove-dupes (list "c" "a" "b" "c")) => (list "a" "b" "c")
;;; DESIGN STRATEGY:  Use observer template for StringList

(define (remove-dupes list)
  (cond [(empty? list) empty]
        [(member? (first list) (rest list)) (rest list)]
        [else (cons (first list) (remove-dupes (rest list)))]))

;;; TESTS:
(begin-for-test
  (check-equal? (remove-dupes (list "c" "a" "b" "c"))
                (list "a" "b" "c")
                "remove-dupes test 1 was not correct"))

          
;;; variables-used-by : ArithmeticExpression -> StringList
;;; GIVEN: an arithmetic expression
;;; RETURNS: a list of the names of all variables used in
;;;     the expression, including variables used in a block
;;;     on the right hand side of its definition or in its body,
;;;     but not including variables defined by a block unless
;;;     they are also used
;;; EXAMPLE:
;;;     (variables-used-by
;;;      (block (var "x")
;;;             (var "y")
;;;             (call (block (var "z")
;;;                          (var "x")
;;;                          (op "+"))
;;;                   (list (block (var "x")
;;;                                (lit 5)
;;;                                (var "x"))
;;;                         (var "x")))))
;;;  => (list "x" "y") or (list "y" "x")
;;; DESIGN STRATEGY: Cases on type of ArithmeticExpression

(define (variables-used-by exp)
  (remove-dupes (cond [(block? exp) (variables-used-by-block exp)]
                      [(call? exp) (variables-used-by-call exp)]
                      [(variable? exp) (list (variable-name exp))]
                      [else (list)])))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-used-by (block (var "x")
                                          (var "y")
                                          (call (block (var "z")
                                                       (var "x")
                                                       (op "+"))
                                                (list (block (var "x")
                                                             (lit 5)
                                                             (var "x"))
                                                      (var "x")))))
                (list "y" "x")
                "variables-used-by test 1 was not correct"))


;;; variables-used-by-block : Block -> StringList
;;; GIVEN: a Block
;;; RETURNS: a list of the names of all variables used in a block on the right
;;;     hand side of its definition or in its body, but not including variables
;;;     defined by a block unless they are also used
;;; EXAMPLE:
;;;     (variables-used-by-block (block (var "x") (lit 5) (var "y"))
;;;                                                    => (list "y")
;;; DESIGN STRATEGY: Use observer template for Block

(define (variables-used-by-block exp)
  (append (variables-used-by (block-rhs exp))
          (variables-used-by (block-body exp))))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-used-by-block (block (var "x") (lit 5) (var "y")))
                (list "y")
                "variables-used-by-block test 1 was not correct"))


;;; variables-used-by-call : Call -> StringList
;;; GIVEN: a Call
;;; RETURNS: a list of the names of all variables used in the expression,
;;;     including variables used in a block on the right hand side of its
;;;     definition or in its body, but not including variables defined by a
;;;     block unless they are also used
;;; EXAMPLE:
;;; (variables-used-by-call (call (block (var "z") (var "x") (op "+"))
;;;                               (list (block (var "x") (lit 5) (var "x"))
;;;                                     (var "y")))   => (list "x" "x" "y")
;;; DESIGN STRATEGY: Use observer template for Call

(define (variables-used-by-call exp)
  (append (variables-used-by (call-operator exp))
          (variables-used-by-call-ops (call-operands exp))))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-used-by-call (call (block (var "z")
                                                     (var "x")
                                                     (op "+"))
                                              (list (block (var "x")
                                                           (lit 5)
                                                           (var "x"))
                                                    (var "y"))))
                (list "x" "x" "y")
                "variables-used-by-call test 1 was not correct"))


;;; variables-used-by-call-ops : ArithmeticExpressionList -> StringList
;;; GIVEN: a list of arithmetic expressions
;;; RETURNS: a list of the names of all variables used in the expression,
;;;     including variables used in a block on the right hand side of its
;;;     definition or in its body, but not including variables defined by a
;;;     block unless they are also used
;;; EXAMPLE:
;;; (variables-used-by-call-ops (list (block (var "x") (lit 5) (var "x"))
;;;                                   (var "y")) => (list "x" "y")
;;; DESIGN STRATEGY: Use observer template for StringList

(define (variables-used-by-call-ops explist)
  (cond [(empty? explist) (list)]
        [else (append (variables-used-by (first explist))
                      (variables-used-by-call-ops (rest explist)))]))

;;; TESTS:
(begin-for-test
  (check-equal? (variables-used-by-call-ops (list (block (var "x")
                                                           (lit 5)
                                                           (var "x"))
                                                    (var "y")))
                (list "x" "y")
                "variables-used-by-call-ops test 1 was not correct"))

          
;;; constant-expression? : ArithmeticExpression -> Boolean
;;; GIVEN: an arithmetic expression
;;; RETURNS: true if and only if the expression is a constant
;;;     expression
;;; EXAMPLES:
;;;     (constant-expression?
;;;      (call (var "f") (list (lit -3) (lit 44))))
;;;         => false
;;;     (constant-expression?
;;;      (call (op "+") (list (var "x") (lit 44))))
;;;         => false
;;;     (constant-expression?
;;;      (block (var "x")
;;;             (var "y")
;;;             (call (block (var "z")
;;;                          (call (op "*")
;;;                                (list (var "x") (var "y")))
;;;                          (op "+"))
;;;                   (list (lit 3)
;;;                         (call (op "*")
;;;                               (list (lit 4) (lit 5)))))))
;;;         => true
;;; DESIGN STRATEGY: Use observer template for ConstantExpression

(define (constant-expression? exp)
  (cond [(literal? exp) true]
        [(call? exp) (constant-expression-call? exp)]
        [(block? exp) (constant-expression? (block-body exp))]
        [else false]))

;;; TESTS:
(begin-for-test
  (check-equal? (constant-expression? (call (var "f") (list (lit -3) (lit 44))))
                false
                "constant-expression? test 1 did not calculate correctly")
  (check-equal? (constant-expression? (call (op "+") (list (var "x") (lit 44))))
                false
                "constant-expression? test 2 did not calculate correctly")
  (check-equal? (constant-expression?
                 (block (var "x")
                        (var "y")
                        (call (block (var "z")
                                     (call (op "*")
                                           (list (var "x")
                                                 (var "y")))
                                     (op "+"))
                              (list (lit 3)
                                    (call (op "*")
                                          (list (lit 4)
                                                (lit 5)))))))
                true
                "constant-expression? test 3 did not calculate correctly"))


;;; constant-expression-call? : Call -> Boolean
;;; GIVEN: a Call
;;; RETURNS: true if and only if the Call is a constant expression
;;; EXAMPLES:
;;;     (constant-expression-call?
;;;      (call (var "f") (list (lit -3) (lit 44))))
;;;         => false
;;;     (constant-expression-call?
;;;      (call (op "+") (list (lit -3) (lit 44))))
;;;         => true
;;; DESIGN STRATEGY: Use observer template for Call

(define (constant-expression-call? exp)
  (and (operation-expression? (call-operator exp))
       (constant-expression-call-operands? (call-operands exp))))

;;; TESTS:
(begin-for-test
  (check-equal? (constant-expression-call? (call (var "f")
                                                 (list (lit -3) (lit 44))))
                false
                "constant-expression-call? test 1 did not calculate correctly")
  (check-equal? (constant-expression-call? (call (op "+")
                                                 (list (lit -3) (lit 44))))
                true
                "constant-expression-call? test 2 did not calculate correctly"))


;;; constant-expression-call-operands? : ArithmeticExpressionList -> Boolean
;;; GIVEN: a list of arithmetic expressions
;;; RETURNS: true if and only if the list contains only constant expressions
;;; EXAMPLES:
;;; (constant-expression-call-operands? (list (lit -3) (lit 44))) => true
;;; (constant-expression-call-operands? (list (var "x") (lit 44))) => false
;;; (constant-expression-call-operands? (list) => true
;;; DESIGN STRATEGY: Use observer template for ArithmeticExpressionList

(define (constant-expression-call-operands? explist)
  (cond [(empty? explist) true]
        [else (and (constant-expression? (first explist))
                   (constant-expression-call-operands? (rest explist)))]))

;;; TESTS:
(begin-for-test
  (check-equal? (constant-expression-call-operands? (list (lit -3) (lit 44)))
                true
                "constant-expression-call-operands? test 1 not correct")
  (check-equal? (constant-expression-call-operands? (list (var "x") (lit 44)))
                false
                "constant-expression-call-operands? test 2 not correct")
  (check-equal? (constant-expression-call-operands? (list))
                true
                "constant-expression-call-operands? test 3 not correct"))


;;; operation-expression? : ArithmeticExpression -> Boolean
;;; GIVEN: an arithmetic expression
;;; RETURNS: true if the arithmetic expression is an operation expression
;;; EXAMPLES:
;;; (operation-expression? (op "+")) => true
;;; (operation-expression? (var "x")) => false
;;; (operation-expression? (block (var "x") (lit 5) (op "+")) => true
;;; DESIGN STRATEGY: Use observer template for OperationExpression

(define (operation-expression? exp)
  (cond [(operation? exp) true]
        [(block? exp) (operation-expression? (block-body exp))]
        [else false]))

;;; TESTS:
(begin-for-test
  (check-equal? (operation-expression? (op "+")) true
                "operation-expression? test 1 not correct")
  (check-equal? (operation-expression? (var "x")) false
                "operation-expression? test 2 not correct")
  (check-equal? (operation-expression? (block (var "x") (lit 5) (op "+"))) true
                "operation-expression? test 3 not correct"))


;;; constant-expression-value : ArithmeticExpression -> Real
;;; GIVEN: an arithmetic expression
;;; WHERE: the expression is a constant expression
;;; RETURNS: the numerical value of the expression
;;; EXAMPLES:
;;;     (constant-expression-value
;;;      (call (op "/") (list (lit 15) (lit 3))))
;;;         => 5
;;;     (constant-expression-value
;;;      (block (var "x")
;;;             (var "y")
;;;             (call (block (var "z")
;;;                          (call (op "*")
;;;                                (list (var "x") (var "y")))
;;;                          (op "+"))
;;;                   (list (lit 3)
;;;                         (call (op "*")
;;;                               (list (lit 4) (lit 5)))))))
;;;         => 23
;;; DESIGN STRATEGY: Cases on type of arithmetic expression

(define (constant-expression-value exp)
  (cond [(literal? exp) (literal-value exp)]
        [(block? exp) (constant-expression-value (block-body exp))]
        [(call? exp) (constant-expression-value-call
                      (operation-name (operator (call-operator exp)))
                      (call-operands exp))]))

;;; TESTS:
(begin-for-test
  (check-equal? (constant-expression-value
                (call (op "*") (list (lit 4) (lit 5) (lit 2))))
                40
                "constant-expression-value test 1 not correct")
  (check-equal? (constant-expression-value
                (call (op "+") (list (lit 3) (lit 2) (lit 1))))
                6
                "constant-expression-value test 2 not correct")
  (check-equal? (constant-expression-value
                (call (op "-") (list (lit 3) (lit 2) (lit 1))))
                0
                "constant-expression-value test 3 not correct")
   (check-equal? (constant-expression-value
                (call (op "/") (list (lit 3) (lit 2) (lit 1))))
                1.5
                "constant-expression-value test 4 not correct")
   (check-equal? (constant-expression-value
                  (call (block (var "z")
                               (call (op "*")
                                     (list (var "x") (var "y")))
                               (op "+"))
                        (list (lit 3)
                              (call (op "*")
                                    (list (lit 4) (lit 5))))))
                23
                "constant-expression-value test 5 not correct")
   (check-equal? (constant-expression-value
                  (block (var "x")
                         (var "y")
                         (call (block (var "z")
                                      (call (op "*")
                                            (list (var "x") (var "y")))
                                      (op "+"))
                               (list (lit 3)
                                     (call (op "*")
                                           (list (lit 4) (lit 5)))))))
                23
                "constant-expression-value test 6 not correct")
   (check-equal? (constant-expression-value
                  (call (op "+")
                        (list (lit 1))))
                 1
                 "contant-expression-value test 7 not correct")
   (check-equal? (constant-expression-value (lit 1))
                 1
                 "contant-expression-value test 8 not correct")
   (check-equal? (constant-expression-value
                  (call (op "+")
                        (list (block (var "z")
                                     (var "x")
                                     (call (op "+")
                                           (list (lit 1)
                                                 (lit 2))))
                              (lit 3))))
                 6
                 "contant-expression-value test 9 not correct"))


;;; operator : OperationExpression -> Operation
;;; GIVEN: an operation expression
;;; RETURNS: the operation of the expression
;;; EXAMPLES:
;;; (operator (op "+")) => (op "+")
;;; (operator (block (var "z") (var "x") (op "+"))) => (op "+")
;;; DESIGN STRATEGY: Use observer template for OperationExpression

(define (operator exp)
  (cond [(operation? exp) exp]
        [(block? exp) (operator (block-body exp))]))

;;; TESTS:
(begin-for-test
  (check-equal? (operator (op "+"))
                (op "+")
                "operator test 1 did not calculate correctly")
  (check-equal? (operator (block (var "z") (var "x") (op "+")))
                (op "+")
                "operator test 2 did not calculate correctly"))


;;; contant-expression-value-call :
;;;                             OperationName ArithmeticExpressionList -> Real
;;; GIVEN: an OperationName and a list of arithmetic expressions
;;; WHERE: the list of arithmetic expressions are all constant expressions
;;; RETURNS: the numerical value of the expression
;;; EXAMPLES:
;;; (contant-expression-value-call "+" (list (lit 1) (lit 2)) => 3
;;; DESIGN STRATEGY: Combine simpler functions

(define (constant-expression-value-call op explist)
  (apply (string-to-operation op)
         (map literal-value-safety (literal-list explist))))

;;; TESTS:
(begin-for-test
  (check-equal? (constant-expression-value-call "+" (list (lit 1) (lit 2)))
                3
                "constant-expression-value-call test 1 not correct"))


;;; literal-list : ArithmeticExpressionList -> ArithmeticExpressionList
;;; GIVEN: a list of arithmetic expressions
;;; WHERE: the list of arithmetic expressions are all constant expressions
;;; RETURNS: a list of literal values of the arithmetic expression list
;;; EXAMPLE:
;;; (literal-list empty) => empty
;;; (literal-list (list (lit 1))) => (list (list 1))
;;; (literal-list (list (call (op "+") (list (lit 1) (lit 2))))) => 3
;;; DESIGN STRATEGY: Use observer template for ArithmeticExpressionList

(define (literal-list exp)
  (cond [(empty? exp) empty]
        [(literal? (first exp)) (cons (first exp)
                                      (literal-list (rest exp)))]
        [(call? (first exp)) (cons (constant-expression-value (first exp))
                                   (literal-list (rest exp)))]
        [(block? (first exp)) (cons (constant-expression-value
                                     (block-body (first exp)))
                                    (literal-list (rest exp)))]))

;;; TESTS:
(begin-for-test
  (check-equal? (literal-list empty)
                empty
                "literal-list test 1 not correct")
  (check-equal? (literal-list (list (lit 1)))
                (list (lit 1))
                "literal-list test 2 not correct")
  (check-equal? (literal-list (list (call (op "+") (list (lit 1) (lit 2)))))
                (list 3)
                "literal-list test 3 not correct"))


;;; literal-value-safety : Literal -> Real
;;; GIVEN: a literal
;;; RETURNS: the value of the literal
;;; EXAMPLE:
;;; (literal-value-safety (lit 3)) => 3
;;; DESIGN STRATEGY: Cases on liberal?

(define (literal-value-safety exp)
  (cond [(literal? exp) (literal-value exp)]
        [else exp]))

;;; TESTS:
(begin-for-test
  (check-equal? (literal-value-safety (lit 3))
                3
                "literal-value-safety test 1 not correct"))


;;; string-to-operation : OperationName -> Operator
;;; GIVEN: an OperationName
;;; RETURNS: the corresponding operator of that OperationName
;;; EXAMPLE:
;;; (string-to-operation "+") => +
;;; DESIGN STRATEGY: Use observer template for OperationName

(define (string-to-operation op)
  (cond [(string=? "+" op) +]
        [(string=? "-" op) -]
        [(string=? "*" op) *]
        [(string=? "/" op) /]))

;;; TESTS:
(begin-for-test
  (check-equal? (string-to-operation "+")
                +
                "string-to-operation test 1 not correct"))
                                            

