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

   ; Check that by default a struct is released when it goes out of scope.
   (lambda ()
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

   ; Check that writing to a struct member doesn't corrupt neighbour members.
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

   ; Struct and pointer type predicates.
   (lambda ()
     (let* ((u (make-uint32pair))
            (u* (uint32pair-pointer u))
            (v (make-vec2))
            (v* (vec2-pointer v)))
       ; Struct predicate returns #t for objects of the same type.
       (assert (vec2? v))
       ; Pointer predicate returns #t for pointers to this object.
       (assert (vec2-pointer? v*))
       ; Struct and pointer predicates don't recognize each other's targets.
       (assert (not (vec2-pointer? v)))
       (assert (not (vec2? v*)))
       ; Instances and pointers of other types are not recognized.
       (assert (not (vec2? u)))
       (assert (not (vec2-pointer? u*)))
       ; Objects that are not instances or pointers at all are not recognized,
       ; and checking them causes no errors.
       (assert (not (vec2? 'not-a-vec2)))
       (assert (not (vec2-pointer? 'not-a-vec2-pointer)))))

   ; Check that we can make an array of a structure.
   (lambda ()
     (let ((a (make-vec2-array 5))
           (a-released #f))
       (assert (vec2-pointer? a))
       ; Check that the array gets released when the last Scheme reference is
       ; lost.
       (make-will a (lambda (x) (set! a-released #t)))
       (set! a #f)
       (gc-voodoo)
       (assert a-released)))
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
