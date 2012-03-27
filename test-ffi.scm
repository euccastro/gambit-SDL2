;   gsc -exe -o test-macro-struct~ -ld-options "-lm" test-macro-struct.scm
;   ./test-macro-struct~

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

;; Code by Oleg Kiselyov
; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

(define-macro (assert expr . others)
  ;; given the list of expressions or vars,
  ;; make the list appropriate for cerr
  (define (make-print-list prefix lst)
    (cond
     ((null? lst) '())
     ((symbol? (car lst))
      (cons #\newline
            (cons (list 'quote (car lst))
                  (cons ": "
			(cons (car lst)
			      (make-print-list #\newline
					       (cdr lst)))))))
     (else
      (cons prefix (cons (car lst) (make-print-list "" (cdr lst)))))))
  ;; return the list of all unique "interesting"
  ;; variables in the expr. Variables that are certain
  ;; to be bound to procedures are not interesting.
  (define (vars-of expr)
    (let loop ((expr expr) (vars '()))
      (cond
       ((not (pair? expr)) vars) ; not an application -- ignore
       ((memq (car expr)
              '(quote let let* letrec let-values* lambda cond quasiquote
                      case define do assert))
        vars) ; won't go there
       (else ; ignore the head of the application
        (let inner ((expr (cdr expr)) (vars vars))
          (cond
           ((null? expr) vars)
           ((symbol? (car expr))
            (inner (cdr expr)
                   (if (memq (car expr) vars) vars (cons (car expr) vars))))
           (else
            (inner (cdr expr) (loop (car expr) vars)))))))))
  (cond
   ((null? others) ; the most common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr nl "bindings"
                            ,@(make-print-list #\newline (vars-of expr)) nl)
                      (error "assertion failure"))))
   ((eq? (car others) 'report:) ; another common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr
                            ,@(make-print-list #\newline (cdr others)) nl)
                      (error "assertion failure"))))
   ((not (memq 'report: others))
    `(or (and ,expr ,@others)
         (begin (cerr "failed assertion: " '(,expr ,@others) nl "bindings"
                      ,@(make-print-list #\newline
                                         (vars-of (cons 'and (cons expr others)))) nl)
                (error "assertion failure"))))
   (else ; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
        `(or (and ,@(reverse exprs))
             (begin (cerr "failed assertion: " ',(reverse exprs)
                          ,@(make-print-list #\newline (cdr reported)) nl)
                    (error "assertion failure"))))
       (else (loop (cons (car reported) exprs) (cdr reported))))))))
    
(define-macro (assure exp error-msg) `(assert ,exp report: ,error-msg))

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
   ))

(define (gc-voodoo)
  (##gc)
  (thread-sleep! .1)
  (##gc)
  (thread-sleep! .1)
  ; For good measure... :/
  (##gc)
  (thread-sleep! .1))

(println)
(for-each (lambda (t)
	    (t)
	    (display "."))
	  tests)
(println)
(println "All OK.")
       
