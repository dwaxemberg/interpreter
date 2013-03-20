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
                                                                               (if (pair? (value (caddar parsetree) environment))
                                                                                       (bind (cadar parsetree)
                                                                                             (value (cadr (caddar parsetree)) (loop (cddar parsetree) (declare (cadar parsetree) 'null environment)))
                                                                                             (loop (cddar parsetree) (declare (cadar parsetree) 'null environment)))
                                                                                       (declare (cadar parsetree) (value (caddar parsetree) environment) environment)))))
                         ((assignment? (car parsetree)) (loop (cdr parsetree) (getEnv (value (car parsetree) environment))))
                         ((return? (car parsetree)) (value (cadar parsetree) environment))
                         ((ifStatement? (car parsetree)) (if (value (cadar parsetree) environment)
                                                             (loop (cons (caddar parsetree) (cdr parsetree)) environment)
                                                             (if (null? (cdddar parsetree))
                                                                 (loop (cdr parsetree) environment)
                                                                 (loop (cons (car (cdddar parsetree)) (cdr parsetree)) environment))))
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
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((symbol? expr) (lookup expr environment))
      ((null? (cdr expr)) (value (car expr) environment))
      ((eq? (operator expr) '=) (bind (operand1 expr) (getVal (value (operand2 expr) environment)) environment))
      ((eq? (operator expr) '+) (+ (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((and (eq? (length expr) 3) (eq? (operator expr) '-)) (- (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '*) (* (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '/) (quotient (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '%) (remainder (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((and (eq? (length expr) 2) (eq? (operator expr) '-)) (- (getVal(value (operand1 expr) environment))))
      ((eq? (operator expr) '>) (> (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '<) (< (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '>=) (>= (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '<=) (<= (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '==) (eq? (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '!=) (not (eq? (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment)))))
      ((eq? (operator expr) '!) (not (getVal (value (operand1 expr) environment))))
      ((eq? (operator expr) '&&) (and (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      ((eq? (operator expr) '||) (or (getVal (value (operand1 expr) environment)) (getVal (value (operand2 expr) environment))))
      (else (error "wat")))))
