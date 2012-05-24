(declare (not proper-tail-calls))

(load "c-test-ffi")

(define-macro (assert expr)
  `(if (not ,expr)
     (begin
       (println "Assertion failed: " (quote ,expr))
       (step)
       (println "OK, moving on..."))))

(define ~
  (let ((epsilon 0.0000001))
    (lambda (x y)
      (< (abs (- x y)) epsilon))))

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

   (lambda ()
     (let* ((parent (make-u32pp))
            (pchild (u32pp-p parent))
            (qchild (u32pp-q parent))
            (qchild-released #f))
       (uint32pair-i-set! pchild #xffff)
       (uint32pair-j-set! pchild #xffff)
       (make-will qchild (lambda (blah) (set! qchild-released #t)))
       (set! qchild #f)
       (gc-voodoo)
       (assert qchild-released)
       (assert (= (uint32pair-i pchild) #xffff))
       (assert (= (uint32pair-j pchild) #xffff))))

   ; Check that the type predicate returns #t only for objects of a given
   ; struct/union type, and that it doesn't error when called with other
   ; struct/union types, or with objects that are not structs or unions at
   ; all.
   (lambda ()
     (let ((u (make-uint32pair))
           (v (make-vec2)))
       (assert (vec2? v))
       (assert (not (vec2? u)))
       (assert (not (vec2? 'not-a-vec2)))))
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
