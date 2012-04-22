;   gsc -exe -o test-macro-struct~ -ld-options "-lm" test-macro-struct.scm
;   ./test-macro-struct~

(declare (debug) (debug-environments) (not proper-tail-calls))

(define-macro (assert expr)
  `(if (not ,expr)
     (begin
       (println "Assertion failed: " (quote ,expr))
       (step)
       (println "OK, moving on..."))))

; Test that ffi-macro.scm behaves well if included more than once.
(include "ffi-macro.scm")
(define old ffi-hierarchical-reference-table)
(include "ffi-macro.scm")
(assert (eq? ffi-hierarchical-reference-table old))

(c-declare #<<c-declare-end

#include <math.h>

typedef struct {___U32 i, j;} uint32pair;
typedef struct {uint32pair p, q;} u32pp;
typedef struct {float x, y;} vec2;
typedef struct {vec2 a, b;} segment;
typedef union {vec2 v; segment s;} vecseg;
typedef struct {vec2 v, *vp;} vec2_and_pointer;
typedef struct vec2_node vec2_node;
struct vec2_node {vec2 v; vec2_node* next;};

void normalize_vec2(vec2* v)
{
    float length = sqrt(v->x * v->x + v->y * v->y);
    v->x /= length;
    v->y /= length;
}

void translate_segment(segment* s, vec2* v)
{
    s->a.x += v->x;
    s->a.y += v->y;
    s->b.x += v->x;
    s->b.y += v->y;
}

c-declare-end
)

(c-struct  uint32pair
          (i unsigned-int32)
          (j unsigned-int32))

(c-struct u32pp
          (p uint32pair voidstar)
          (q uint32pair voidstar))

(c-struct vec2 (x float) (y float))
(c-struct segment (a vec2 voidstar) (b vec2 voidstar))
(c-union vecseg
  (v vec2 voidstar)
  (s segment voidstar))
(c-struct (vec2-and-pointer vec2_and_pointer)
  (v vec2 voidstar)
  (vp vec2* pointer))
(c-struct (vec2-node vec2_node)
  (v vec2 voidstar)
  ((lets-call-this-scheme-next-just-for-the-sake-of-testing next)
   ((pointer vec2-node) vec2_node*)
   pointer))

(define vec2-normalize!
  (c-lambda ((pointer vec2)) void "normalize_vec2"))

(define segment-translate!
  (c-lambda ((pointer segment) (pointer vec2)) void
	    "translate_segment"))

(define vec2-data
  (c-lambda (vec2) scheme-object
    "___result = ___EXT(___data_rc(___arg1_voidstar));"))
