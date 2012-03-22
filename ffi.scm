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
  (define (c-native struct-or-union type . fields)
    (let* 
      ((type-name (symbol->string type))
       (attr-worker
         (lambda (fn)
           (lambda (field)
             (let* ((attr-name (symbol->string (car field)))
                    (attr-type (cadr field))
                    (access-type (if (null? (cddr field)) 
                                   'scalar 
                                   (caddr field)))
                    (voidstar (eq? access-type 'voidstar))
                    (pointer (eq? access-type 'pointer)))
               (fn attr-name 
                   (if pointer 
                     (string->symbol
                       (string-append (symbol->string attr-type) "*"))
                     attr-type)
                   voidstar
                   pointer)))))
       (accessor
         (attr-worker
           (lambda (attr-name attr-type voidstar pointer)
             (let ((_voidstar (if (or voidstar pointer)
                               "_voidstar" 
                               ""))
                   (amperstand (if voidstar "&" "")))
               `(define ,(string->symbol 
                           (string-append type-name "-" attr-name))
                  (c-lambda (,type) ,attr-type
                            ,(string-append
                               "___result" 
                               _voidstar 
                               " = "
                               amperstand
                               "((("
                               type-name
                               "*)___arg1_voidstar)->"
                               attr-name
                               ");")))))))
       (mutator
         (attr-worker
           (lambda (attr-name attr-type voidstar pointer)
             (let ((_voidstar (if (or voidstar pointer) "_voidstar" ""))
                   (cast 
                     (cond 
                       (voidstar
                        (string-append "(" (symbol->string attr-type) "*)"))
                       (pointer
                        (string-append "(" (symbol->string attr-type) ")"))
                       (else "")))
                   (dereference (if voidstar "*" "")))
             `(define ,(string->symbol
                         (string-append type-name "-" attr-name "-set!"))
                (c-lambda (,type ,attr-type) void
                          ,(string-append
                             "(*(" 
                             type-name
                             "*)___arg1_voidstar)."
                             attr-name
                             " = "
                             dereference
                             cast
                             "___arg2"
                             _voidstar 
                             ";"))))))))
      (append
        `(begin
           (c-define-type ,type (,struct-or-union ,type-name))
           (c-define-type ,(string->symbol (string-append type-name "*"))
                          (pointer ,type))
           (define ,(string->symbol (string-append "make-" type-name))
             ; Constructor.
             (c-lambda () ,type
                       ,(string-append
                          "___result_voidstar = "
                          "___EXT(___alloc_rc)(sizeof(" type-name "));")))
           (define ,(string->symbol (string-append type-name "-pointer"))
             ; Take pointer.
             (c-lambda (,type) (pointer ,type)
                       "___result_voidstar = ___arg1_voidstar;"))
           (define ,(string->symbol (string-append "pointer->" type-name))
             ; Pointer dereference
             (c-lambda ((pointer ,type)) ,type
                       "___result_voidstar = ___arg1_voidstar;")))
        (map accessor fields)
        (map mutator fields)))))

(define-macro 
  (c-struct type . fields)
  (apply c-native 'struct type fields))

(define-macro
  (c-union type . fields)
  (apply c-native 'union type fields))
