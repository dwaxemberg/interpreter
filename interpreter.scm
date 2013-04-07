; Written by Diego Waxemberg & Aaron Neyer

(load "loopSimpleParser.scm")

; main function, loops through the parse tree and calls functions to deal with the tuples.
(define interpret
  (lambda (filename)
      (call/cc (lambda (return) (interpret-statement-list (parser filename) (declare 'return return '()))))))

(define interpret-statement-list
  (lambda (parsetree environment)
    (cond
      ((null? parsetree) environment)
      (else (interpret-statement-list (cdr parsetree) (interpret-statement (car parsetree) environment))))))

(define interpret-statement
  (lambda (stmt environment)
    (cond
      ((operator? stmt 'var) (declare-stmt stmt environment))
      ((operator? stmt '=) (getEnv (value stmt environment)))
      ((operator? stmt 'return) ((lookup 'return environment) (fixBool (getVal (value (cadr stmt) environment)))))
      ((operator? stmt 'while) (while-stmt stmt environment))
      ((operator? stmt 'break) ((lookup 'break environment) (cddr environment)))
      ((operator? stmt 'begin) (cdr (interpret-statement-list (cdr stmt) (cons '() environment))))
      ((operator? stmt 'if) (if-stmt stmt environment)))))

(define if-stmt
  (lambda (stmt environment)
      (if (getVal (value (cadr stmt) environment))
        (interpret-statement (caddr stmt) (getEnv (value (cadr stmt) environment)))
        (if (null? (cdddr stmt))
          (getEnv (value (cadr stmt) environment))
          (interpret-statement (cadddr stmt) (getEnv (value (cadr stmt) environment)))))))

(define while-stmt
  (lambda (stmt environment)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body env)
                               (if (getVal (value condition env))
                                 (loop condition body (interpret-statement body (declare-continuation 'break break env)))
                                 env))))
                 (loop (cadr stmt) (caddr stmt) environment))))))

(define declare-stmt
  (lambda (stmt environment)
    (if (null? (cddr stmt))
      (declare (cadr stmt) 'null environment)
      (declare (cadr stmt) (getVal (value (caddr stmt) environment)) (getEnv (value (caddr stmt) environment))))))

; pops a stack off the environment
(define popFrame
  (lambda (environment)
    (cond
      ((or (null? (car environment))(pair? (caar environment))(eq? (caar environment) 'break)) (cdr environment))
      (else environment))))

; checks if the expression is a keyword
(define operator?
  (lambda (expr op)
    (cond
      ((not (pair? expr)) #f)
      (else (eq? (operator expr) op)))))

; changes #t and #f to true and false respectively
(define fixBool
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (else val))))

; looks up a name in the environment and returns the value associated with it
(define lookup
  (lambda (name environment)
    (cond
      ((null? environment) 'none)
      ((or (null? (car environment)) (list? (caar environment))) (lookup name (append (car environment) (cdr environment))))
      ((eq? (caar environment) name) (cadar environment))
      (else (lookup name (cdr environment))))))

(define lookupWithErr
  (lambda (name environment)
    (let ((val (lookup name environment)))
      (cond
        ((eq? val 'none) (error "You must declare a variable before using it"))
        ((eq? (lookup name environment) 'null) (error "You must assign a variable before using it"))
        (else val)))))

; add a value declaration to the environment
(define declare
  (lambda (name value environment)
    (cond
      ((not (eq? (lookup name environment) 'none))(error "You cannot redefine a variable!"))
      ((null? environment) (cons (cons name (cons value '())) environment))
      ((or (null? (car environment)) (list? (caar environment))) (cons (declare name value (car environment))(cdr environment)))
      (else (cons (cons name (cons value '())) environment)))))

; allows redeclaring of variables, for continuations
(define declare-continuation
  (lambda (name value environment)
    (cond
      ((null? environment) (cons (cons name (cons value '())) environment))
      ((or (null? (car environment)) (list? (caar environment))) (cons (declare name value (car environment)) (cdr environment)))
      (else (cons (cons name (cons value '())) environment)))))

; binds a value to a variable in the environment
(define bind
  (lambda (name value environment)
    (cond
      ((eq? (lookup name environment) 'none) (error "You must declare a variable before assigning it"))
      (else (makeTuple value (reassign name value environment))))))

; reassign a variable in the environment and return the environment
(define reassign
  (lambda (name value environment)
    (cond
      ((null? (car environment)) (cons (car environment) (reassign name value(cdr environment))))
      ((list? (caar environment)) (if (eq? (getVal (tryReassign name value (car environment))) #t)
                                                                     (cons (getEnv (tryReassign name value (car environment))) (cdr environment))
                                                                     (cons (car environment) (reassign name value (cdr environment)))))
      ((null? environment) (error "You did something very wrong."))
      ((eq? (caar environment) name) (cons (makeTuple name value) (cdr environment)))
      (else (cons (car environment) (reassign name value (cdr environment)))))))

(define tryReassign
  (lambda (name value environment)
    (cond
      ((null? environment) '())
      ((eq? (caar environment) name) (makeTuple #t (cons (makeTuple name value) (cdr environment))))
      (else (cons (car environment) (tryReassign name value (cdr environment)))))))
; gets the operator portion of a tuple
(define operator
  (lambda (expr)
    (car expr)))

; gets the first operand portion of a tuple
(define operand1
  (lambda (expr)
    (cadr expr)))

; gets the second operand portion of a tuple
(define operand2
  (lambda (expr)
    (caddr expr)))

; turns two values into a tuple
(define makeTuple
  (lambda (arg1 arg2)
    (cons arg1 (cons arg2 '()))))

; takes a tuple containing a value and an environment and returns the value
(define getVal
  (lambda (tup)
    (if (pair? tup) (car tup) tup)))

; takes a tuple containing a value and an environmment and returns the environment
(define getEnv
  (lambda (tup)
    (if (pair? tup) (cadr tup) tup)))

(define binaryOp
  (lambda (f expr environment)
      (makeTuple (f (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment)))))))

(define myand
  (lambda (a b)
    (and a b)))

(define myor
  (lambda (a b)
    (or a b)))

; recursively evaluates an expression and returns the resulting environment
(define value
  (lambda (expr environment)
    (cond
      ((number? expr) (makeTuple expr environment))
      ((eq? expr 'true) (makeTuple #t environment))
      ((eq? expr 'false) (makeTuple #f environment))
      ((symbol? expr) (makeTuple (lookupWithErr expr environment) environment))
      ((null? (cdr expr)) (getVal (value (car expr) environment)))
      ((eq? (operator expr) '=) (bind (operand1 expr) (getVal (value (operand2 expr) environment)) (getEnv (value (operand2 expr) environment))))
      ((eq? (operator expr) '+) (binaryOp + expr environment))
      ((and (eq? (length expr) 3) (eq? (operator expr) '-)) (binaryOp - expr environment))
      ((eq? (operator expr) '*) (binaryOp * expr environment))
      ((eq? (operator expr) '/) (binaryOp quotient expr environment))
      ((eq? (operator expr) '%) (binaryOp remainder expr environment))
      ((and (eq? (length expr) 2) (eq? (operator expr) '-)) (makeTuple (- (getVal(value (operand1 expr) environment))) environment))
      ((eq? (operator expr) '>) (binaryOp > expr environment))
      ((eq? (operator expr) '<) (binaryOp < expr environment))
      ((eq? (operator expr) '>=) (binaryOp >= expr environment))
      ((eq? (operator expr) '<=) (binaryOp <= expr environment))
      ((eq? (operator expr) '==) (binaryOp eq? expr environment))
      ((eq? (operator expr) '!=) (makeTuple (not (eq? (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment)))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '!) (makeTuple (not (getVal (value (operand1 expr) environment))) (getEnv (value (operand1 expr) environment))))
      ((eq? (operator expr) '&&) (binaryOp myand expr environment))
      ((eq? (operator expr) '||) (binaryOp myor expr environment))
      (else (error "wat")))))
