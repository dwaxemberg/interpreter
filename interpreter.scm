; Written by Diego Waxemberg & Aaron Neyer

(load "loopSimpleParser.scm")

; main function, loops through the parse tree and calls functions to deal with the tuples.
(define interpret
  (lambda (parsetree)
      (letrec ((loop (lambda (parsetree environment)
                       (cond
                         ((null? parsetree) environment)
                         ((operator? (car parsetree) 'var) (loop (cdr parsetree) (if (null? (cddar parsetree))
                                                                             (declare (cadar parsetree) 'null environment)
                                                                             (declare (cadar parsetree)
                                                                                      (getVal (value (caddar parsetree) environment))
                                                                                      (getEnv (value (caddar parsetree) environment))))))
                         ((operator? (car parsetree) '=) (loop (cdr parsetree) (getEnv (value (car parsetree) environment))))
                         ((operator? (car parsetree) 'return) (fixBool (getVal (value (cadar parsetree) environment))))
                         
                         ((operator? (car parsetree) 'if) (if (getVal (value (cadar parsetree) environment))
                                                             (loop (cons (caddar parsetree) (cdr parsetree)) (getEnv (value (cadar parsetree) environment)))
                                                             (if (null? (cdddar parsetree))
                                                                 (loop (cdr parsetree) (getEnv (value (cadar parsetree) environment)))
                                                                 (loop (cons (car (cdddar parsetree)) (cdr parsetree)) (getEnv (value (cadar parsetree) environment))))))
                         ))))
        (loop parsetree '()))))

; checks if the expression is a keyword
(define operator?
  (lambda (expr op)
    (cond
      ((not (pair? expr)) #f)
      ((else (eq? (car expr) op))))))

; checks if the expression is a while statement
(define while? 
  (lambda (expr)
    (cond
      ((not (pair? expr)) #f)
      ((else (eq? (car expr) 'while))))))

; checks if the expression is a declaration
(define declare?
  (lambda (expr)
    (cond
      ((not (pair? expr)) #f)
      (else (eq? (car expr) 'var)))))

(define fixBool
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (else val))))

; checks if the expression is a return
(define return?
  (lambda (expr)
    (cond
      ((not (pair? expr)) #f)
      (else (eq? (car expr) 'return)))))

; checks if the expression is an if statement
(define ifStatement?
  (lambda (expr)
    (cond
      ((not (pair? expr)) #f)
      (else (eq? (car expr) 'if)))))

; looks up a name in the environment and returns the value associated with it
(define lookup
  (lambda (name environment)
    (cond
      ((null? environment) 'none)
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
    (if (eq? (lookup name environment) 'none)
        (cons (cons name (cons value '())) environment)
        (error "You cannot redefine a variable!"))))

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
      ((null? environment) (error "You did something very wrong."))
      ((eq? (caar environment) name) (cons (cons name (cons value '())) (cdr environment)))
      (else (cons (car environment) (reassign name value (cdr environment)))))))

; checks if something is an assignment
(define assignment?
  (lambda (expr)
    (eq? '= (operator expr))))

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
    (if (list? tup) (car tup) tup)))

; takes a tuple containing a value and an environmment and returns the environment
(define getEnv
  (lambda (tup)
    (if (list? tup) (cadr tup) tup)))

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
