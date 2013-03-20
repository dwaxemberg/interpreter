; Written by Diego Waxemberg & Aaron Neyer

(load "verySimpleParser.scm")

; main function, loops through the parse tree and calls functions to deal with the tuples.
(define interpret
  (lambda (parsetree)
      (letrec ((loop (lambda (parsetree environment)
                       (cond
                         ((null? parsetree) environment)
                         ((declare? (car parsetree)) (loop (cdr parsetree) (if (null? (cddar parsetree))
                                                                             (declare (cadar parsetree) 'null environment)
                                                                             (declare (cadar parsetree)
                                                                                      (getVal (value (caddar parsetree) environment))
                                                                                      (getEnv (value (caddar parsetree) environment))))))
                         ((assignment? (car parsetree)) (loop (cdr parsetree) (getEnv (value (car parsetree) environment))))
                         ((return? (car parsetree)) (getVal (value (cadar parsetree) environment)))
                         ((ifStatement? (car parsetree)) (if (getVal (value (cadar parsetree) environment))
                                                             (loop (cons (caddar parsetree) (cdr parsetree)) (getEnv (value (cadar parsetree) environment)))
                                                             (if (null? (cdddar parsetree))
                                                                 (loop (cdr parsetree) (getEnv (value (cadar parsetree) environment)))
                                                                 (loop (cons (car (cdddar parsetree)) (cdr parsetree)) (getEnv (value (cadar parsetree) environment))))))
                         ))))
        (loop parsetree '()))))

; checks if the expression is a declaration
(define declare?
  (lambda (expr)
    (cond
      ((not (pair? expr)) #f)
      (else (eq? (car expr) 'var)))))

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
      ((null? environment) #f)
      ((eq? (caar environment) name) (cadar environment))
      (else (lookup name (cdr environment))))))

; add a value declaration to the environment
(define declare
  (lambda (name value environment)
    (cons (cons name (cons value '())) environment)))

; binds a value to a variable in the environment
(define bind
  (lambda (name value environment)
    (if (lookup name environment)
      (makeTuple value (cons (cons name (cons value '())) environment))
      (error "You must declare a variable before assigning it"))))

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

; recursively evaluates an expression and returns the resulting environment
(define value
  (lambda (expr environment)
    (cond
      ((number? expr) (makeTuple expr environment))
      ((eq? expr 'true) (makeTuple #t environment))
      ((eq? expr 'false) (makeTuple #f environment))
      ((symbol? expr) (makeTuple (lookup expr environment) environment))
      ((null? (cdr expr)) (getVal (value (car expr) environment)))
      ((eq? (operator expr) '=) (bind (operand1 expr) (getVal (value (operand2 expr) environment)) (getEnv (value (operand2 expr) environment))))
      ((eq? (operator expr) '+) (makeTuple (+ (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))) environment))
      ((and (eq? (length expr) 3) (eq? (operator expr) '-)) (makeTuple (- (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))) environment))
      ((eq? (operator expr) '*) (makeTuple (* (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))) environment))
      ((eq? (operator expr) '/) (makeTuple (quotient (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))) environment))
      ((eq? (operator expr) '%) (makeTuple (remainder (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))) environment))
      ((and (eq? (length expr) 2) (eq? (operator expr) '-)) (makeTuple (- (getVal(value (operand1 expr) environment))) environment))
      ((eq? (operator expr) '>) (makeTuple (> (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '<) (makeTuple (< (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '>=) (makeTuple (>= (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '<=) (makeTuple (<= (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '==) (makeTuple (eq? (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '!=) (makeTuple (not (eq? (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment)))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '!) (makeTuple (not (getVal (value (operand1 expr) environment))) (getEnv (value (operand1 expr) environment))))
      ((eq? (operator expr) '&&) (makeTuple (and (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      ((eq? (operator expr) '||) (makeTuple (or (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) (getEnv (value (operand1 expr) environment))))) (getEnv (value (operand2 expr) (getEnv (value (operand1 expr) environment))))))
      (else (error "wat")))))
