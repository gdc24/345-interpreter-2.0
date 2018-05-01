; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
; #lang racket
; (require "simpleParser.scm")
(load "classParser.scm")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file classname)
    (scheme->language
     (call/cc
      (lambda (return)
        (execute-main-class classname
                            (outer-layer (parser file)
                                         (newenvironment)
                                         (lambda (v env) (myerror "Uncaught exception thrown")))
                            return
                            (lambda (env) (myerror "Break used outside of loop"))
                            (lambda (env) (myerror "Continue used outside of loop"))
                            (lambda (v env) (myerror "Uncaught exception thrown"))))))))

(define first-statement car)
(define remaining-statements cdr)

; outer layer
(define outer-layer
  (lambda (stmt-list environment throw)
    (if (null? stmt-list)
        environment
        (outer-layer (remaining-statements stmt-list)
                     (create-class-closure (first-statement stmt-list) environment throw)
                     throw))))


; class layer of interpreter
; reads classes
(define class-layer
  (lambda (statement-list environment throw)
    (cond
      ((eq? '() statement-list) environment)
      ((eq? 'var (statement-type (first-statement statement-list))) (outer-layer (remaining-statements statement-list) (interpret-declare (first-statement statement-list) environment throw) throw))
      ((or (eq? 'function (statement-type (first-statement statement-list)))
           (eq? 'static-function (statement-type (first-statement statement-list))))
       (class-layer (remaining-statements statement-list) (insert-function (first-statement statement-list) environment throw) throw))
      (else (class-layer (remaining-statements statement-list)
                         (create-class-closure (first-statement statement-list) environment)
                         throw)))))
;      (else (myerror "Unsupported top level statement: " (statement-type statement))))))


(define execute-main-class
  (lambda (classname environment return break continue throw)
    (lookup-class classname environment return break continue throw)))

(define get-closure-instances cdar)

(define lookup-class
  (lambda (classname environment return break continue throw)
    (execute-main-function (get-closure-instances (lookup-closure classname environment))
                           return break continue throw)))

(define lookup-closure
  (lambda (className environment)
    (lookup-class-closure className (get-class-closure-names environment) (get-class-closure-vals environment))))

;;;;;;;;;;;;;;;;;;
(define class-closure-list
  (lambda (environment)
    (car environment)))

(define get-class-closure-names
  (lambda (environment)
    (car (class-closure-list environment))))

(define get-class-closure-vals
  (lambda (environment)
    (cdr (class-closure-list environment))))
;;;;;;;;;;;;;;;;;


;(define get-class-closure-names
;  (lambda (environment)
;    (caar environment)))

;(define get-class-closure-vals
;  (lambda (environment)
;    (cdar environment)))

(define lookup-class-closure
  (lambda (classname vars vals)
    (cond
      ((null? vars) (myerror "Class does not exist: " classname))
      ((eq? (string->symbol classname) (car vars)) (car vals)) ;; wrong;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (else (lookup-class-closure classname (cdr vars) (cdr vals))))))


    

; looks up and executes main
(define execute-main-function
  (lambda (environment return break continue throw)
    (interpret-statement-list (get-function-body 'main environment throw)
                              (push-frame environment)
                              return break continue throw)))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (remaining-statements statement-list)
                                  (interpret-statement (first-statement statement-list) environment return break continue throw)
                                  return break continue throw))))

(define interpret-statement-list-environment
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list-environment (remaining-statements statement-list)
                                              (interpret-statement-environment (first-statement statement-list) environment return break continue throw)
                                              return break continue throw))))

(define interpret-statement-environment
  (lambda (statement environment return break continue throw)
    (if (eq? 'return (statement-type statement))
        (return environment)
        (interpret-statement statement environment return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw))
      ((eq? 'new (statement-type statement)) (create-instance-closure statement environment throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw))
      ((and (eq? '= (statement-type statement))
            (list? (caddr statement))
            (eq? 'funcall (caaddr statement)))
       (interpret-assign statement (interpret-function (caddr statement) environment throw) throw)) ;;??????
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw))
      ((eq? 'funcall (statement-type statement)) (interpret-function statement environment throw))
      ((eq? 'function (statement-type statement)) (insert-function statement environment throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw)
    (return (eval-expression (get-expr statement) environment throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw return break continue)
    (cond
      ((and (exists-declare-value? statement) (list? (get-declare-value statement)))
       (create-instance-closure statement environment return break continue throw))
      ((exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment))
      (else (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (interpret-statement-list (cdr statement)
                              (push-frame environment)
                              return
                              (lambda (env) (break (pop-frame env)))
                              (lambda (env) (continue (pop-frame env)))
                              (lambda (v env) (throw v (pop-frame env))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

(define interpret-funcall
  (lambda (statement enviornment)
    (call/cc
     (lambda (func-return)
       (eval-funcall statement environment)))))

; Interprets a function
; returns final state including input function
;(define interpret-function
;  (lambda (statement environment throw)
;    (insert (get-func-name statement) (create-func-closure statement environment) environment)))

(define interpret-function
  (lambda (funcall environment throw)
    (call/cc
     (lambda (func-return)
       (interpret-function-helper funcall environment func-return
                                  (lambda (env) (myerror "Break used outside of loop"))
                                  (lambda (env) (myerror "Continue used outside of loop"))
                                  throw)))))

; statement-list interpreter for when you want to return states instead of values
(define interpret-statement-list-for-env
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list-for-env (cdr statement-list)
                                          (interpret-statement-for-env (car statement-list) environment return break continue throw)
                                          return break continue throw))))



(define get-func-closure
  (lambda (name environment throw)
    (eval-expression name environment throw)))



(define interpret-function-helper
  (lambda (expr environment return break continue throw)
    (interpret-statement-list-environment (get-function-body (get-func-name expr) environment throw)
                                          (get-scope (get-func-name expr) (get-func-closure (get-func-name expr) environment throw) expr environment throw)
                                          return break continue throw)))
        

(define insert-function
  (lambda (statement environment throw)
    (insert (cadr statement)
            (create-func-closure statement
                                 (lambda
                                     (name closure call environment throw)
                                   (generate-func-environment name closure call environment throw)))
            environment)))

(define generate-func-environment
  (lambda (name closure expr environment throw)
    (cons (bind-params name environment expr throw)
          (get-layers-in-scope name environment))))

(define bind-params
  (lambda (name environment expr throw)
    (if (null? (cddr expr))
        (newframe)
        (list (car (get-function-closure name environment throw))
              (eval-params (cddr expr) environment throw)))))

(define first-param car)
(define remaining-params cdr)

(define eval-params
  (lambda (param-list environment throw)
    (if (null? param-list)
        '()
        (cons (eval-expression (first-param param-list) environment throw)
              (eval-params (remaining-params param-list) environment throw)))))


; creates a closure for the function, containing the following:
; 1. function parameters
; 2. function body
; 3. scope for the function (the state the function is executed in)
;(define create-func-closure
;  (lambda (statement current-environment)
;    (if (null? (cadr statement))
;        (myerror "invalid function")

(define create-func-closure
  (lambda (statement current-environment)
    (list (get-func-params statement)
          (get-func-body statement)
          current-environment)))

; create a class closure
; 1. the parent class
; 2. the list of instance fields
; 3. the list of methods/function names and closures
(define create-class-closure
  (lambda (stmt current-environment throw)
    (insert (get-class-name stmt) ; var = class name
            (cons (get-extends stmt) ; val = (parent (instances/fields/functions etc))
                  (class-layer (get-class-body stmt) (newenvironment) throw))
            current-environment)))        ; environment = same environment that was passed in

;;;;;;;;;;;;;;;;;;;;;
;(define create-instance-closure
;  (lambda (stmt current-environment throw)
;    (find-class-closure (get-class-name stmt) environment throw)))
(define find-class-closure
  (lambda (name environment throw)
    (lookup-class-closure name (get-class-closure-names environment) (get-class-closure-vals environment))));
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-true-type cdaddr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define create-instance-closure
  (lambda (statement environment return break continue throw)
    (cond
      ((null? statement) (myerror "Statement doesn't exist"))
      ((null? (find-class-closure (get-true-type statement) environment throw)) (myerror "Class doesn't exist"))
      (else (insert (car statement) (make-statelayer-from-instance-fields (get-closure-of-class (lookup (get-new-class-name statement) environment)) (newenvironment) return break continue throw) environment)))))

(define make-statelayer-from-instance-fields
  (lambda (class-closure environment return break continue throw)
    (cond
      ((null? class-closure) environment)
      ((list? (car class-closure)) (make-statelayer-from-instance-fields (cdr class-closure) (interpret-statement (car class-closure) environment return break continue throw) return break continue throw)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(define create-class-closure
;  (lambda (statement current-environment)
;    (list (get-parent-class statement)
;          (get-instance-fields statement)
;          current-environment)))

(define add-class-closure
  (lambda (statement environment)
    (insert (get-class-name statement)
            (create-class-closure statement environment)
            environment)))

(define get-class-name cadr) ; (class B (extends A) body) => B
(define get-extends caddr) ; (class B (extends A) body) => (extends A)
(define get-parent-class-from-closure cdar) ; ((extends a) (instances/etc)) => a
;(define get-parent-class   ; (class B (extends A) body) => a
;  (lambda (stmt)
;    (cadr (get-extends stmt))))
(define get-class-body cadddr) ; (class B (extends A) body) => body

;(define get-instance-fields
;  (lambda (stmt)
;    (if (list
;         (cadddr stmt)) ; (class B (extends A) body) => body


; create instance closure
; 1. the instance's class (i.e. the run-time type or the true type)
; 2. a list of instance field values


(define func-body cadr)

(define get-function-body
  (lambda (func-name environment throw)
    (func-body (get-function-closure func-name environment throw))))

(define get-function-closure
  (lambda (func-name environment throw)
    (eval-expression func-name environment throw)))

; given function name, return function closure
(define func-closure
  (lambda (func-name environment)
;    (eval-expression func-name environment throw)))
    (lookup func-name environment)))

; given function name, return function body
;(define func-body
;  (lambda (func-name environment throw)
;    (cadr (func-closure func-name environment))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment throw)))))


(define eval-function
  (lambda (expr environment throw)
    (call/cc
     (lambda (func-return)
       (eval-function-helper expr environment func-return
                             (lambda (env) (myerror "Break used outside of loop"))
                             (lambda (env) (myerror "Continue used outside of loop"))
                             throw)))))

(define eval-funcall
  (lambda (expr environment)
    (interpret-statement-list)))

(define eval-function-helper
  (lambda (expr environment return break continue throw)
    (interpret-statement-list (get-function-body (get-func-name expr) environment throw)
                              (get-scope (get-func-name expr) (get-function-closure (get-func-name expr) environment throw) expr environment throw)
                              return break continue throw)))

(define func-names caar)
(define remaining-frames cdr)

; gets the layer w/ global function and variable declarations
(define get-layers-in-scope
  (lambda (func-name environment)
    (cond
      ((null? (remaining-frames environment)) environment)
      ((exists-in-list? func-name (func-names environment)) environment)
      (else (get-layers-in-scope func-name (remaining-frames environment))))))


(define closure-scope caddr)

(define get-scope
  (lambda (name closure call environment throw)
    ((closure-scope (get-function-closure name environment throw)) name closure call environment throw)))

;(define get-scope
;  (lambda (func-name closure call environment throw)
;    (get-environment func-name environment)))

(define get-environment
  (lambda (func-name environment)
    (caddr (func-closure func-name environment))))

(define get-param
  (lambda (func-name environment)
    (car (func-closure func-name environment))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? 'funcall (operator expr)) (eval-function expr environment throw))
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define get-func-name cadr)
(define get-func-params caddr)
(define get-func-body cadddr)
(define get-first-statement car)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

    


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

