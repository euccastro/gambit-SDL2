;   gsc -exe -o test-macro-struct~ -ld-options "-lm" test-macro-struct.scm
;   ./test-macro-struct~

(include "ffi-macro.scm")

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

(define-macro (assert expr . others)
  `(if (not ,expr)
     (raise (quote ("Assertion failed:" ,expr)))))

(define ~
  (let ((epsilon 0.0000001))
    (lambda (x y)
      (< (abs (- x y)) epsilon))))

(define vec2-data
  (c-lambda (vec2) scheme-object
    "___result = ___EXT(___data_rc(___arg1_voidstar));"))

(define tests
  (list
   ; Simplest test, make a vec2 and read its members.
   (lambda ()
     (let ((v (make-vec2))
	   (x 1.0)
	   (y 2.0))
       (vec2-x-set! v x)
       (vec2-y-set! v y)
       (assert (~ (vec2-x v) x))
       (assert (~ (vec2-y v) y))))
   (lambda ()
     ; Check that by default a struct is released when it goes out of
     ; scope.
     (let ((vs-released #f))
	(let ((vs (make-vecseg)))
	  (make-will vs (lambda (blah)
			  (set! vs-released #t))))
	(gc-voodoo)
	(assert vs-released)))
   ; Check that a struct will no be released while a child holds a
   ; reference to it, but it will be released when the child itself
   ; goes away.
   (lambda ()
     (let* ((parent-released #f)
	    (child-released #f)
            (parent (make-vecseg))
	    (child (vecseg-v parent)))
	 (make-will parent (lambda (blah) (set! parent-released #t)))
	 (make-will child (lambda (blah) (set! child-released #t)))
	 (set! parent #f)
	 (gc-voodoo)
	 (assert (not parent-released))
	 (assert (not child-released))
	 (set! child #f)
	 (gc-voodoo)
	 (assert child-released)
	 (assert parent-released)))
   ))

(define (gc-voodoo)
  (##gc)
  (thread-sleep! 0.001)
  (##gc)
  (thread-sleep! 0.001)
  ; For good measure... :/
  (##gc)
  (thread-sleep! 0.001))

(println)
(for-each (lambda (t)
	    (t)
	    (display "."))
	  tests)
(println)
(println "All OK.")
