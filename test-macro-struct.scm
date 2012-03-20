; Compile & test: 
;   gsc -exe -o test-macro-struct -ld-options "-lm" test-macro-struct.scm
;   ./test-macro-struct

(include "ffi.scm")

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

(c-struct vec2 (x float #f) (y float #f))

(define vec2-normalize!
  (c-lambda ((pointer vec2)) void "normalize_vec2"))

(define (test)
  (letrec ((v (make-vec2))
           (v* (vec2-pointer v)))
    (vec2-x-set! v 5.0)
    (vec2-y-set! v 10.0)
    (println "v has x=" (vec2-x v) " and y=" (vec2-y v) ".")
    (vec2-normalize! v*)
    (println "v has x=" (vec2-x v) " and y=" (vec2-y v) ".")))

(test)
