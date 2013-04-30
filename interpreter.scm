; Written by Diego Waxemberg & Aaron Neyer

(load "classParser.scm")

(define base_env '())
(define class_env '(()))

(define currentclass 'none)

; main function, loops through the parse tree and calls functions to deal with the tuples.
(define interpret
  (lambda (filename classname)
    (fixBool (call/cc (lambda (return)
                        (interpret-statement-list (append (parser filename) (list (list 'funcall (list 'dot classname 'main)))) base_env))))))

(define interpret-statement-list
  (lambda (parsetree environment)
    (cond
      ((null? parsetree) environment)
      (else (interpret-statement-list (cdr parsetree) (interpret-statement (car parsetree) environment))))))

(define println
  (lambda (todisplay)
    (display todisplay)
    (display "\n")))

(define interpret-statement
  (lambda (stmt environment)
    (cond
      ((operator? stmt 'throw) (throw-stmt stmt environment))
      ((operator? stmt 'var) (declare-stmt stmt environment))
      ((operator? stmt 'static-var) (declare-stmt stmt environment))
      ((operator? stmt 'class) (declare-class stmt environment))
      ((operator? stmt 'new) (declare-new stmt environment))
      ((operator? stmt '=) (begin (value stmt environment) environment))
      ((operator? stmt 'return) ((lookup 'return environment) (value (cadr stmt) environment)))
      ((operator? stmt 'while) (while-stmt stmt environment))
      ((operator? stmt 'break) ((lookup 'break environment) (cddr environment)))
      ((operator? stmt 'continue) ((lookup 'continue environment) environment))
      ((operator? stmt 'begin) (pop-stack (begin-stmt stmt environment)))
      ((operator? stmt 'function) (function-stmt stmt environment))
      ((operator? stmt 'static-function) (function-stmt stmt environment))
      ((operator? stmt 'funcall) (let ((funcreturn (funcall-stmt stmt environment)))
                                   (if (or (eq? (cadr stmt) 'main) (and (list? (cadr stmt)) (eq? (caddr (cadr stmt)) 'main))) funcreturn environment)))
      ((operator? stmt 'if) (if-stmt stmt environment))
      (else (error (string-append "Unregonized statement: " (format "~a" (operator stmt))))))))

; throws an exception  
(define throw-stmt
  (lambda (stmt environment)
    (error (cadr stmt))))
  
; declares a new instance
(define declare-new
  (lambda (stmt environment)
    (let ((env (cons (list (car stmt) (box class_env)) environment)))
      (set-box! (lookupClass 'new env) (unbox (lookupClass (cadr stmt) env))) (display env))))

; declares a new class in the environment
(define declare-class
  (lambda (stmt environment)
    (set! currentclass (cadr stmt))
    (let ((env (cons (list (cadr stmt) (box class_env)) environment)))
      (begin (interpret-statement-list (cadddr stmt) env)
             env))))

(define funcall-stmt
  (lambda (stmt environment)
    (define func (cadr stmt))
    (define funcBody 'none)
    (if (symbol? func) (set! funcBody (lookup func environment)) (set! funcBody (unbox (dotLookup (caddr func) environment (cadr func)))))
    (call/cc (lambda (return)
               (if (eq? (numArguments (cddr stmt)) (numArguments (car funcBody)))
                 (interpret-statement-list (cadr funcBody)
                                           (declare-multiple (car funcBody)
                                                             (cddr stmt) (declare-continuation 'return return (add-stack environment))))
                 (error "Wrong number of arguments"))))))

(define numArguments
  (lambda (args)
    (cond
      ((null? args) 0)
      ((eq? (car args) '&) (numArguments (cdr args)))
      (else (+ 1 (numArguments (cdr args)))))))

(define declare-multiple
  (lambda (variables vals environment)
    (cond
      ((null? variables) environment)
      ((eq? (car variables) '&) (declare-multiple (cddr variables) (cdr vals) (declare-continuation (cadr variables) (car vals) environment)))
      (else (declare-multiple (cdr variables) (cdr vals) (declare-continuation (car variables) (value (car vals) environment) environment))))))

(define function-stmt
  (lambda (stmt environment)
    (declare (cadr stmt) (cddr stmt) environment)))

(define begin-stmt
  (lambda (stmt environment)
    (call/cc (lambda (continue)
               (interpret-statement-list (cdr stmt) (declare-continuation 'continue continue (add-stack environment)))))))

(define add-stack
  (lambda (environment)
    (let ((env (lookupClass currentclass environment)))
      (begin (set-box! env (cons '() (unbox env))) environment))))

(define pop-stack
  (lambda (environment)
    (let ((env (lookupClass currentclass environment)))
      (begin (set-box! env (cdr (unbox env))) environment))))

(define if-stmt
  (lambda (stmt environment)
    (if (value (cadr stmt) environment)
      (interpret-statement (caddr stmt) environment)
      (if (null? (cdddr stmt))
        environment
        (interpret-statement (cadddr stmt) environment)))))

(define while-stmt
  (lambda (stmt environment)
    (call/cc (lambda (break) (while-loop (cadr stmt) (caddr stmt) (declare-continuation 'break break environment))))))

(define while-loop
  (lambda (condition body env)
    (if (value condition env)
      (while-loop condition body (interpret-statement body env))
      env)))

(define declare-stmt
  (lambda (stmt environment)
    (if (null? (cddr stmt))
      (declare (cadr stmt) 'null environment)
      (declare (cadr stmt) (value (caddr stmt) environment) environment))))

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
    (let ((env (lookupClass currentclass environment)))
      (cond
        ((not (eq? (lookup name environment) 'none)) (error "You cannot redefine a variable!"))
        (else (begin (set-box! env (cons (cons (list name (box value)) (car (unbox env))) (cdr (unbox env)))) environment))))))

; allows redeclaring of variables, for continuations
(define declare-continuation
  (lambda (name value environment)
    (let ((env (lookupClass currentclass environment)))
      (begin (set-box! env (cons (cons (list name (box value)) (car (unbox env))) (cdr (unbox env)))) environment))))

; binds a value to a variable in the environment
(define bind
  (lambda (name value environment)
    (cond
      ((eq? (lookup name environment) 'none) (error "You must declare a variable before assigning it"))
      (else (reassign name value environment)))))

(define reassign
  (lambda (name value environment)
    (begin
      (set-box! (lookupBox name environment) value)
      value)))

(define flatten-once
  (lambda (l)
    (cond
      ((null? l) l)
      (else (append (car l) (flatten-once (cdr l)))))))

(define lookupClass
  (lambda (name environment)
    (cond
      ((null? environment) '())                                                                           
      ((eq? (caar environment) name)(cadar environment))
      (else (lookupClass name (cdr environment))))))

(define dotLookup
  (lambda (name environment class)
    (let ((env
           (unbox (if (null? (lookupClass class environment))
               (lookupClass class (flatten-once (unbox (lookupClass currentclass environment))))
               (lookupClass class environment)))))
      (_lookupBox name (flatten-once env)))))

; looks up a name in the environment and returns the value associated with it
(define lookup
  (lambda (name environment)
    (unbox (lookupBox name environment))))

(define lookupBox
  (lambda (name environment)
    (let ((env (unbox (lookupClass currentclass environment))))
      (let ((lookupResult (unbox (_lookupBox name (flatten-once env)))))
        (if (and (symbol? lookupResult) (not (eq? lookupResult 'none)) (not (eq? lookupResult 'null)))
          (lookupBox lookupResult env)
          (_lookupBox name (flatten-once env)))))))

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
(define operator car)

; gets the first operand portion of a tuple
(define operand1 cadr)

; gets the second operand portion of a tuple
(define operand2 caddr)

(define binaryOp
  (lambda (f expr environment)
    ;(println expr)
    ;(println environment)
    (f (value (operand1 expr) environment) (value (operand2 expr) environment))))

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
      ((operator? expr 'new) (unbox (lookupClass (cadr expr) environment)))
      ((operator? expr 'funcall) (funcall-stmt expr environment))
      ((operator? expr 'dot) (unbox (dotLookup (caddr expr) environment (cadr expr))))
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((symbol? expr) (lookupWithErr expr environment))
      ((null? (cdr expr)) (value (car expr) environment))
      ((eq? (operator expr) '=) (bind (operand1 expr) (value (operand2 expr) environment) environment))
      ((eq? (operator expr) '+) (binaryOp + expr environment))
      ((and (eq? (length expr) 3) (eq? (operator expr) '-)) (binaryOp - expr environment))
      ((eq? (operator expr) '*) (binaryOp * expr environment))
      ((eq? (operator expr) '/) (binaryOp quotient expr environment))
      ((eq? (operator expr) '%) (binaryOp remainder expr environment))
      ((and (eq? (length expr) 2) (eq? (operator expr) '-)) (- (value (operand1 expr) environment)))
      ((eq? (operator expr) '>) (binaryOp > expr environment))
      ((eq? (operator expr) '<) (binaryOp < expr environment))
      ((eq? (operator expr) '>=) (binaryOp >= expr environment))
      ((eq? (operator expr) '<=) (binaryOp <= expr environment))
      ((eq? (operator expr) '==) (binaryOp eq? expr environment))
      ((eq? (operator expr) '!=) (not (eq? (value (operand1 expr) environment) (value (operand2 expr) environment))))
      ((eq? (operator expr) '!) (not (value (operand1 expr) environment)))
      ((eq? (operator expr) '&&) (binaryOp myand expr environment))
      ((eq? (operator expr) '||) (binaryOp myor expr environment))
      (else (error (string-append "wat? " (format "~a" expr)))))))
