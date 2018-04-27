(load "interpreter2-callcc-no-boxes.scm")

(define tests
  (lambda ()
    (if (and
         (eq? (interpret "tests3/test01.txt") 10)     ; main with code
         (eq? (interpret "tests3/test02.txt") 14)     ; function that uses global variables
         (eq? (interpret "tests3/test03.txt") 45)     ; changes global variables
         (eq? (interpret "tests3/test04.txt") 55)     ; recursive function
;         (eq? (interpret "tests3/test05.txt") 1)      ; functions with multiple parameters that hide global variables
;         (eq? (interpret "tests3/test06.txt") 115)    ; verifying that your code uses static scoping instead of dynamic scoping.
;         (eq? (interpret "tests3/test07.txt") 'true)  ; boolean parameters and return values
;         (eq? (interpret "tests3/test08.txt") 20)     ; multiple function calls in an expression
;         (eq? (interpret "tests3/test09.txt") 24)     ; function call in the parameter of a function
;         (eq? (interpret "tests3/test10.txt") 2)      ; function call that ignores the return value
;         (eq? (interpret "tests3/test11.txt") 35)     ; function without a return statement
;         (eq? (interpret "tests3/test13.txt") 90)     ; functions inside functions
;         (eq? (interpret "tests3/test14.txt") 69)     ; functions inside functions accessing variables outside
;         (eq? (interpret "tests3/test15.txt") 87)     ; functions inside functions with variables of the same name
         )
         #t
         #f)))