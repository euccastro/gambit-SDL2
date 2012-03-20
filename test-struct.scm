; Compile & test: 
;   gsc -exe -o test-struct -ld-options "-lm" test-struct.scm
;   ./test-struct

(c-declare #<<c-declare-end

#include <math.h>

typedef struct {float x, y;} vec2;

void normalize_vec2(vec2* v)
{
    float length = sqrt(v->x * v->x + v->y * v->y);
    v->x /= length;
    v->y /= length;
}

c-declare-end
)

(c-define-type vec2 (struct "vec2"))

(define make-vec2
  (c-lambda () vec2 
    "___result_voidstar = ___EXT(___alloc_rc)(sizeof(vec2));"))

(define vec2-normalize!
  (c-lambda ((pointer vec2)) void "normalize_vec2"))

(define vec2-pointer
  (c-lambda (vec2) (pointer vec2) 
    "___result_voidstar = ___arg1_voidstar;"))

(define vec2-x-set!
  (c-lambda (vec2 float) void 
    "((vec2*)___arg1_voidstar)->x = ___arg2;"))

(define vec2-y-set!
  (c-lambda (vec2 float) void 
    "((vec2*)___arg1_voidstar)->y = ___arg2;"))

(define vec2-x
  (c-lambda (vec2) float 
    "___result = ((vec2*)___arg1_voidstar)->x;"))

(define vec2-y
  (c-lambda (vec2) float 
    "___result = ((vec2*)___arg1_voidstar)->y;"))

(define (test)
  (letrec ((v (make-vec2))
           (v* (vec2-pointer v)))
    (vec2-x-set! v 5.0)
    (vec2-y-set! v 10.0)
    (println "v has x=" (vec2-x v) " and y=" (vec2-y v) ".")
    (vec2-normalize! v*)
    (println "v has x=" (vec2-x v) " and y=" (vec2-y v) ".")))

(test)
