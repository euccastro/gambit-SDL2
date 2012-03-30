(c-declare #<<c-declare-end

___SCMOBJ leave_alone(void *p)
{
    ___EXT(___set_data_rc)(p, 0);
    p = NULL;
    ___EXT(___release_rc)(p);
    return ___FIX(___NO_ERR);
}

c-declare-end
)


; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-August/003781.html
(define-macro (at-expand-time-and-runtime . exprs)
  (let ((l `(begin ,@exprs)))
    (eval l)
    l))

(define-macro (at-expand-time . expr)
  (eval (cons 'begin expr)))

;; Creating the bindings in a simple C function makes for more compact
;; binaries, as per Marc Feeley's advice.
;;
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-February/005688.html
;; (Via 'Alvaro Castro-Castilla).

(define-macro
  (c-constants . names)
  (define (interval lo hi)
    (if (< lo hi) (cons lo (interval (+ lo 1) hi)) '()))
  (let ((nb-names (length names))
        (wrapper (gensym)))
    `(begin
       (define ,wrapper
         (c-lambda (int)
                   int
                   ,(string-append
                      "static int _tmp_[] = {\n"
                      (apply string-append
                             (map (lambda (i name)
                                    (let ((name-str (symbol->string name)))
                                      (string-append
                                        (if (> i 0) "," "")
                                        name-str)))
                                  (interval 0 nb-names)
                                  names))
                      "};\n"
                      "___result = _tmp_[___arg1];\n")))
       ,@(map (lambda (i name)
                `(define ,name (,wrapper ,i)))
              (interval 0 nb-names)
              names))))

(at-expand-time
  (define unmanaged-prefix "__um_")
  (define (c-native struct-or-union type . fields)
    (let* 
      ((scheme-type (if (pair? type) (car type) type))
       (c-type (if (pair? type) (cadr type) type))
       (scheme-type-name (symbol->string scheme-type))
       (c-type-name (symbol->string c-type))
       (attr-worker
         (lambda (fn)
           (lambda (field)
             (let* ((attr (car field))
                    (scheme-attr-name (symbol->string (if (pair? attr) 
                                                        (car attr)
                                                        attr)))
                    (c-attr-name (symbol->string (if (pair? attr)
                                                   (cadr attr)
                                                   attr)))
                    (attr-type (cadr field))
                    (scheme-attr-type (if (pair? attr-type)
                                        (car attr-type)
                                        attr-type))
                    (c-attr-type (if (pair? attr-type)
                                   (cadr attr-type)
                                   attr-type))
                    (access-type (if (null? (cddr field)) 
                                   'scalar 
                                   (caddr field)))
                    (voidstar (eq? access-type 'voidstar))
                    (pointer (eq? access-type 'pointer)))
               (fn scheme-attr-name
                   c-attr-name
                   scheme-attr-type
                   c-attr-type
                   voidstar
                   pointer)))))
       (accessor
	(attr-worker
	 (lambda (scheme-attr-name c-attr-name scheme-attr-type c-attr-type 
				   voidstar pointer)
	   (let ((_voidstar (if (or voidstar pointer) "_voidstar" ""))
		 (amperstand (if voidstar "&" ""))
		 (scheme-attr-type (if voidstar
                                       (string->symbol
					(string-append 
					 unmanaged-prefix
					 (symbol->string scheme-attr-type)))
                                       scheme-attr-type)))
	     `(define (,(string->symbol 
			 (string-append scheme-type-name 
					"-" 
					scheme-attr-name))
		       parent)
		(let ((ret
		       ((c-lambda (,scheme-type) ,scheme-attr-type
				  ,(string-append
				    "___result" _voidstar
				    ; XXX: correctly cast to type, should help with enums in C++.
				    ;" = (" (symbol->string c-attr-type) ")"
				    " = "
				    amperstand "(((" c-type-name "*)___arg1_voidstar)->"
				    c-attr-name ");"))
			parent)))
		  ,@(if voidstar
			`(((c-lambda
			    (,scheme-attr-type scheme-object) void
			    "___EXT(___set_data_rc)(___arg1_voidstar, ___arg2);")
			   ret parent))
			'())
		  ret))))))
       (mutator
         (attr-worker
           (lambda (scheme-attr-name c-attr-name scheme-attr-type c-attr-type 
                                     voidstar pointer)
             (let ((_voidstar (if (or voidstar pointer) "_voidstar" ""))
                   (cast 
                     (cond 
                       (voidstar
                         (string-append "(" (symbol->string c-attr-type) "*)"))
                       (pointer
                         (string-append "(" (symbol->string c-attr-type) ")"))
		       ; XXX: cast primitive types too, should help with enums in C++
                       (else "")))
                   (dereference (if voidstar "*" "")))
               `(define ,(string->symbol
                           (string-append 
                             scheme-type-name "-" scheme-attr-name "-set!"))
                  (c-lambda (,scheme-type ,scheme-attr-type) void
                            ,(string-append
                               "(*(" 
                               c-type-name
                               "*)___arg1_voidstar)."
                               c-attr-name
                               " = "
                               dereference
                               cast
                               "___arg2"
                               _voidstar 
                               ";"))))))))
      (append
        `(begin
           (c-define-type ,scheme-type (,struct-or-union ,c-type-name ,c-type))
           ; Unmanaged version of structure.
           (c-define-type ,(string->symbol 
                             (string-append unmanaged-prefix scheme-type-name))
                          (,struct-or-union ,c-type-name ,c-type
                           "leave_alone"))
           (c-define-type ,(string->symbol (string-append scheme-type-name "*"))
                          (pointer ,scheme-type))
           (define ,(string->symbol (string-append "make-" scheme-type-name))
             ; Constructor.
             (c-lambda () ,scheme-type
                       ,(string-append
                          "___result_voidstar = "
                          "___EXT(___alloc_rc)(sizeof(" c-type-name "));")))
           (define ,(string->symbol 
                      (string-append scheme-type-name "-pointer"))
             ; Take pointer.
             (c-lambda (,scheme-type) (pointer ,scheme-type)
                       "___result_voidstar = ___arg1_voidstar;"))
           (define ,(string->symbol 
                      (string-append "pointer->" scheme-type-name))
             ; Pointer dereference
             (c-lambda ((pointer ,scheme-type)) ,scheme-type
                       "___result_voidstar = ___arg1_voidstar;")))
        (map accessor fields)
        (map mutator fields)))))

(define-macro 
  (c-struct type . fields)
  (apply c-native 'struct type fields))

(define-macro
  (c-union type . fields)
  (apply c-native 'union type fields))