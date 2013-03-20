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
                         ((expression? (car parsetree)) (loop (cdr parsetree) (value (car parsetree) environment)))
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
        (if (pair? value)
            (bind name (cadar value) value)
            (cons (cons name (cons value '())) environment))
        (error "You must declare a variable before assigning it"))))

; checks if something is an expression
(define expression?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((number? expr) #t)
      ((symbol? expr) #t)
      ((boolean? expr) #t)
      ((not (pair? expr)) #f)
      ((null? (cdr expr)) (expression? (car expr)))
      ((= (length expr) 3)
       (and (or (eq? '= (operator expr)) 
                (eq? '+ (operator expr)) 
                (eq? '- (operator expr)) 
                (eq? '* (operator expr)) 
                (eq? '/ (operator expr)) 
                (eq? '% (operator expr))
                (eq? '== (operator expr))
                (eq? '!= (operator expr))
                (eq? '< (operator expr))
                (eq? '> (operator expr))
                (eq? '<= (operator expr))
                (eq? '>= (operator expr))
                (eq? '&& (operator expr))
                (eq? '|| (operator expr))
                ) 
            (expression? (operand1 expr))
            (expression? (operand2 expr))))
      ((= (length expr) 2)
       (and (or (eq? '- (operator expr)) (eq? '! (operator expr))) (expression? (operand1 expr))))
      (else #f))))

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

; recursively evaluates an expression and returns the resulting environment
(define value
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((symbol? expr) (lookup expr environment))
      ((null? (cdr expr)) (value (car expr) environment))
      ((eq? (operator expr) '=) (bind (operand1 expr) (value (operand2 expr) environment) environment))
      ((eq? (operator expr) '+) (+ (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((and (eq? (length expr) 3) (eq? (operator expr) '-)) (- (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '*) (* (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '/) (quotient (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '%) (remainder (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((and (eq? (length expr) 2) (eq? (operator expr) '-)) (- (value (operand1 expr) environment)))
      ((eq? (operator expr) '>) (> (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '<) (< (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '>=) (>= (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '<=) (<= (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '==) (eq? (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '!=) (not (eq? (value (operand1 expr) environment) (value (operand2 expr) environment))))
      ((eq? (operator expr) '!) (not (value (operand1 expr) environment)))
      ((eq? (operator expr) '&&) (and (value (operand1 expr) environment) (value (operand2 expr) environment)))
      ((eq? (operator expr) '||) (or (value (operand1 expr) environment) (value (operand2 expr) environment)))
      (else (error "wat")))))
