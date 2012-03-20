
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

