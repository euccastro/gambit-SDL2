(define make-int*
  (c-lambda () (pointer int)
    "___result_voidstar = ___EXT(___alloc_rc)(sizeof(int));\n"))

(define dereference-write-int*
  (c-lambda ((pointer int) int) void
    "*(int*)___arg1_voidstar = ___arg2;"))

(define dereference-read-int*
  (c-lambda ((pointer int)) int
    "___result = *(int*)___arg1_voidstar;"))
