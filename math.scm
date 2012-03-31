; C <math.h> functions, wrapped here just for the sake of running the
; example in test.scm

(c-declare "#include <math.h>")

(define sin
  (c-lambda (double) double "sin"))

(define cos
  (c-lambda (double) double "cos"))

(define sqrt
  (c-lambda (double) double "sqrt"))

(define M_PI
  ((c-lambda () double "___result = M_PI;")))
