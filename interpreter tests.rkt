; We want to test this code with SchemeUnit. We start by creating a file called file-test.scm to contain our tests.
; At the top of file-test.scm we import SchemeUnit and file.scm:

  #lang scheme/base

(require schemeunit
         "interpreter2-tail-recursion-no-boxes.scm")

; Now we add some tests to check our library:

  (check-equal? (interpret "") 2 "description")
  (check-equal? (interpret "") 2 "description")
