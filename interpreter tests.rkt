; We want to test this code with SchemeUnit. We start by creating a file called file-test.scm to contain our tests.
; At the top of file-test.scm we import SchemeUnit and file.scm:

;  #lang scheme/base

(load schemeunit
         "interpreter2-tail-recursion-no-boxes.scm")

; Now we add some tests to check our library:

(check-equal? (interpret "tests1/test01.txt") 150 "description")
(check-equal? (interpret "tests1/test02.txt") -4 "description")
(check-equal? (interpret "tests1/test03.txt") 10 "")
(check-equal? (interpret "tests1/test04.txt") 16 "")

