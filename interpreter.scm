; Written by Diego Waxemberg & Aaron Neyer

(load "functionParser.scm")

; main function, loops through the parse tree and calls functions to deal with the tuples.
(define interpret
  (lambda (filename)
      (call/cc (lambda (return) (interpret-statement-list (append (parser filename) '((funcall main))) (declare-continuation 'return return '(())))))))

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
      ((operator? stmt 'continue) ((lookup 'continue environment) environment))
      ((operator? stmt 'begin) (begin-stmt stmt environment))
      ((operator? stmt 'function) (function-stmt stmt environment))
      ((operator? stmt 'funcall) (let ((funcreturn (funcall-stmt stmt environment))) (if (eq? (cadr stmt) 'main) funcreturn environment)))
      ((operator? stmt 'if) (if-stmt stmt environment))
      (else (error (string-append "Unregonized statement: " (format "~a" (operator stmt))))))))

(define funcall-stmt
  (lambda (stmt environment)
    (call/cc (lambda (return)
               (interpret-statement-list (cadr (lookup (cadr stmt) environment))
                                         (declare-multiple (car (lookup (cadr stmt) environment))
                                                           (cddr stmt) (declare-continuation 'return return (add-stack environment))))))))

(define declare-multiple
  (lambda (variables vals environment)
    (cond
      ((null? variables) environment)
      ((eq? (car variables) '&) (declare-multiple (cddr variables) (cdr vals) (declare-continuation (cadr variables) (car vals) environment)))
      (else (declare-multiple (cdr variables) (cdr vals) (declare-continuation (car variables) (getVal (value (car vals) environment)) environment))))))

(define function-stmt
  (lambda (stmt environment)
    (declare (cadr stmt) (cddr stmt) environment)))

(define begin-stmt
  (lambda (stmt environment)
    (call/cc (lambda (continue)
               (cdr (interpret-statement-list (cdr stmt) (declare-continuation 'continue continue (add-stack environment))))))))

(define add-stack
  (lambda (environment)
    (cons '() environment)))

(define pop-stack
  (lambda (environment)
    (cdr environment)))

(define if-stmt
  (lambda (stmt environment)
    (let ((valenv (value (cadr stmt) environment)))
      (if (getVal valenv)
        (interpret-statement (caddr stmt) (getEnv valenv))
        (if (null? (cdddr stmt))
          (getEnv valenv)
          (interpret-statement (cadddr stmt) (getEnv valenv)))))))

(define while-stmt
  (lambda (stmt environment)
    (call/cc (lambda (break) (while-loop (cadr stmt) (caddr stmt) (declare-continuation 'break break environment))))))

(define while-loop
  (lambda (condition body env)
    (let ((valenv (value condition env)))
      (if (getVal (getVal valenv))
        (while-loop condition body (interpret-statement body (getEnv valenv)))
        (getEnv valenv)))))

(define declare-stmt
  (lambda (stmt environment)
    (if (null? (cddr stmt))
      (declare (cadr stmt) 'null environment)
      (let ((valenv (value (caddr stmt) environment)))
        (declare (cadr stmt) (getVal valenv) (getEnv valenv))))))

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

; add a value declaration to the environment
(define declare
  (lambda (name value environment)
    (cond
      ((not (eq? (lookup name environment) 'none)) (error "You cannot redefine a variable!"))
      (else (cons (cons (makeTuple name (box value)) (car environment)) (cdr environment))))))

; allows redeclaring of variables, for continuations
(define declare-continuation
  (lambda (name value environment)
    (cons (cons (makeTuple name (box value)) (car environment)) (cdr environment))))

; binds a value to a variable in the environment
(define bind
  (lambda (name value environment)
    (cond
      ((eq? (lookup name environment) 'none) (error "You must declare a variable before assigning it"))
      (else (makeTuple value (reassign name value environment))))))

(define reassign
  (lambda (name value environment)
    (set-box! (lookupBox name environment) value) environment))

(define flatten-once
  (lambda (l)
    (cond
      ((null? l) l)
      (else (append (car l) (flatten-once (cdr l)))))))

; looks up a name in the environment and returns the value associated with it
(define lookup
  (lambda (name environment)
    (unbox (lookupBox name environment)))) 
        
(define lookupBox
  (lambda (name environment)
    (let ((lookupResult (unbox (_lookupBox name (flatten-once environment)))))
    (if (and (symbol? lookupResult) (not (eq? lookupResult 'none))) 
        (lookupBox lookupResult environment)
        (_lookupBox name (flatten-once environment))))))
    
(define _lookupBox
  (lambda (name environment)
    (cond
    ((null? environment) (box 'none))
    ((eq? (caar environment) name) (cadar environment))
    (else (_lookupBox name (cdr environment))))))

(define lookupWithErr
  (lambda (name environment)
    (let ((val (lookup name environment)))
      (cond
        ((eq? val 'none) (error "You must declare a variable before using it"))
        ((eq? val 'null) (error "You must assign a variable before using it"))
        (else val)))))

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
      (let ((valenv1 (value (operand1 expr) environment)) (valenv2 (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) 
                   (makeTuple (f (getVal valenv1) (getVal valenv2)) 
                   (getEnv valenv2)))))

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
      ((operator?  expr 'funcall) (funcall-stmt expr environment))
      ((number? expr) (makeTuple expr environment))
      ((eq? expr 'true) (makeTuple #t environment))
      ((eq? expr 'false) (makeTuple #f environment))
      ((symbol? expr) (makeTuple (lookupWithErr expr environment) environment))
      ((null? (cdr expr)) (getVal (value (car expr) environment)))
      ((eq? (operator expr) '=) (let ((valenv (value (operand2 expr) environment))) (bind (operand1 expr) (getVal valenv) (getEnv valenv))))
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
      ((eq? (operator expr) '!) (let ((valenv (value (operand1 expr) environment))) (makeTuple (not (getVal valenv)) (getEnv valenv))))
      ((eq? (operator expr) '&&) (binaryOp myand expr environment))
      ((eq? (operator expr) '||) (binaryOp myor expr environment))
      (else (error "wat")))))
