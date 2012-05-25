(include "ffi-macro#.scm")
(namespace ("ffi#"))
(##include "~~/lib/gambit#.scm")

(c-declare #<<c-declare-end

#ifndef FFI_MACRO_LEAVE_ALONE
#define FFI_MACRO_LEAVE_ALONE

#include <malloc.h>

___SCMOBJ leave_alone(void *p)
{
    return ___FIX(___NO_ERR);
}

#endif

c-declare-end
)

(define hierarchical-reference-table
  (if (table? hierarchical-reference-table)
    hierarchical-reference-table
    (make-table weak-keys: #t weak-values: #f test: eq?)))

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
  (define (to-string x)
    (cond ((string? x) x)
          ((symbol? x) (symbol->string x))
          (else (error "Unsupported type: " x))))
  (define (mixed-append . args) (apply string-append (map to-string args)))
  (define (symbol-append . args)
    (string->symbol (apply mixed-append args)))
  (define unmanaged-prefix "unmanaged-")
  (define (c-native struct-or-union type . fields)
    (let*
      ((scheme-type (if (pair? type) (car type) type))
       (c-type (if (pair? type) (cadr type) type))
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
                                       (symbol-append unmanaged-prefix
                                                      scheme-attr-type)
                                       scheme-attr-type)))
               `(define (,(symbol-append scheme-type
                                         "-"
                                         scheme-attr-name)
                          parent)
                  (let ((ret
                          ((c-lambda
                             (,scheme-type) ,scheme-attr-type
                             ,(string-append
                                "___result" _voidstar
                                ; XXX: correctly cast to type, should help with enums in C++.
                                ;" = (" (symbol->string c-attr-type) ")"
                                " = "
                                amperstand "(((" c-type-name
                                "*)___arg1_voidstar)->"
                                c-attr-name ");"))
                           parent)))
                    ,@(if voidstar
                        '((table-set!
                            ffi#hierarchical-reference-table ret parent)
                          (make-will
                            ret
                            (lambda (x)
                              (table-set!
                                ffi#hierarchical-reference-table x))))
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
                         (mixed-append "(" c-attr-type "*)"))
                       (pointer
                         (mixed-append "(" c-attr-type ")"))
                       ; XXX: cast primitive types too, should help with enums in C++
                       (else "")))
                   (dereference (if voidstar "*" "")))
               `(define ,(symbol-append
                           scheme-type "-" scheme-attr-name "-set!")
                  (c-lambda
                    (,scheme-type ,scheme-attr-type) void
                    ,(string-append
                       "(*(" c-type-name "*)___arg1_voidstar)." c-attr-name
                       " = " dereference cast "___arg2" _voidstar ";"))))))))
      (append
        `(begin
           (c-define-type ,scheme-type (,struct-or-union ,c-type-name ,c-type))
           ; Unmanaged version of structure.
           (c-define-type ,(symbol-append unmanaged-prefix scheme-type)
                          (,struct-or-union ,c-type-name ,c-type "leave_alone"))
           (c-define-type ,(symbol-append scheme-type "*")
                          (pointer ,scheme-type))
           (define ,(symbol-append "make-" scheme-type)
             ; Constructor.
             (c-lambda
               () ,scheme-type
               ,(string-append "___result_voidstar = malloc(sizeof(" c-type-name "));")))
           (define (,(symbol-append scheme-type "?") x)
             ; Type predicate.
             (and (foreign? x) (memq (quote ,c-type) (foreign-tags x)) #t))
           (define (,(symbol-append scheme-type "-pointer?") x)
             ; Pointer type predicate.
             (and (foreign? x)
                  (memq (quote ,(symbol-append "struct " c-type "*"))
                        (foreign-tags x))
                  #t))
           (define ,(symbol-append scheme-type "-pointer")
             ; Take pointer.
             (c-lambda
               (,scheme-type) (pointer ,scheme-type)
               "___result_voidstar = ___arg1_voidstar;"))
           (define ,(symbol-append "pointer->" scheme-type)
             ; Pointer dereference
             (c-lambda
               ((pointer ,scheme-type)) ,scheme-type
               "___result_voidstar = ___arg1_voidstar;"))
           (define (,(symbol-append "make-" scheme-type "-array") len)
             (let ((ret ((c-lambda
                           (int) (pointer ,scheme-type)
                           ,(string-append
                             "___result_voidstar = malloc(___arg1 * sizeof(" c-type-name "));"))
                         len)))
               ret)))
        (map accessor fields)
        (map mutator fields)))))

(define-macro
  (c-struct type . fields)
  (apply c-native 'struct type fields))

(define-macro
  (c-union type . fields)
  (apply c-native 'union type fields))

(namespace (""))
(include "ffi-macro#.scm")
