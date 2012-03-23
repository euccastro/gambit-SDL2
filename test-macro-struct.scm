; Compile & test: 
;   gsc -exe -o test-macro-struct~ -ld-options "-lm" test-macro-struct.scm
;   ./test-macro-struct~
;
;XXX: allow a different scheme type name.
;XXX: test pointer->struct.

(include "ffi.scm")

(c-declare #<<c-declare-end

#include <math.h>

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

(c-struct vec2 (x float) (y float))
(c-struct segment (a vec2 voidstar) (b vec2 voidstar))
(c-union vecseg (v vec2 voidstar) (s segment voidstar))
(c-struct vec2_and_pointer (v vec2 voidstar) (vp vec2* pointer))
(c-struct vec2_node (v vec2 voidstar) (next vec2_node* pointer))

(define vec2-normalize!
  (c-lambda ((pointer vec2)) void "normalize_vec2"))

(define segment-translate!
  (c-lambda ((pointer segment) (pointer vec2)) void "translate_segment"))

(define (test)
  (let* ((v (make-vec2))
         (v* (vec2-pointer v))
         (s (make-segment))
         (s* (segment-pointer s))
         (sa (segment-a s))
         (sa* (vec2-pointer sa))
         (sb (segment-b s))
         (vs (make-vecseg))
         (vs* (vecseg-pointer vs))
         (vsv (vecseg-v vs))
         (vsv* (vec2-pointer vsv))
         (vss (vecseg-s vs))
         (vssa (segment-a vss))
         (vssa* (vec2-pointer vssa))
         (vssb (segment-b vss))
         (vssb* (vec2-pointer vssb))
         (p (make-vec2_and_pointer))
         (pv (vec2_and_pointer-v p))
         (pvp (vec2_and_pointer-vp p))
         (n (make-vec2_node))
         (n* (vec2_node-pointer n))
         (nv (vec2_node-v n))
         (nn (vec2_node-next n))
         (n2 (make-vec2_node))
         (n2* (vec2_node-pointer n2))
         (n2v (vec2_node-v n2))
         (n2n (vec2_node-next n2)))
    (vec2-x-set! v 5.0)
    (vec2-y-set! v 10.0)
    (println "v has x=" (vec2-x v) " and y=" (vec2-y v) ".")
    (vec2-normalize! v*)
    (println "v has x=" (vec2-x v) " and y=" (vec2-y v) ".")
    (vec2-x-set! sa 1.0)
    (vec2-y-set! sa 2.0)
    (vec2-x-set! sb 3.0)
    (vec2-y-set! sb 4.0)
    (println "s si ((" (vec2-x sa) "," (vec2-y sa)
             "), (" (vec2-x sb) "," (vec2-y sb) ").")
    (segment-b-set! s v)
    (println "s si ((" (vec2-x sa) "," (vec2-y sa)
             "), (" (vec2-x sb) "," (vec2-y sb) ").")
    (vecseg-s-set! vs s)
    (println "vs si ((" (vec2-x vssa) "," (vec2-y vssa)
             "), (" (vec2-x vssb) "," (vec2-y vssb) ").")
    (vecseg-v-set! vs v)
    (println "vs si ((" (vec2-x vssa) "," (vec2-y vssa)
             "), (" (vec2-x vssb) "," (vec2-y vssb) ").")
    (vec2_and_pointer-vp-set! p v*)
    (println "pvp " (vec2-x (pointer->vec2 (vec2_and_pointer-vp p))))
    (vec2-x-set! nv 33.)
    (vec2-y-set! nv 44.)
    (vec2_node-next-set! n2 n*)
    (let* ((next (pointer->vec2_node (vec2_node-next n2)))
           (next-v (vec2_node-v next)))
      (println "next-v has x=" (vec2-x next-v) " and y=" (vec2-y next-v) "."))
    ))

(test)
