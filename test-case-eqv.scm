(c-declare #<<c-declare-end

int a=1;
unsigned int b=1; 

c-declare-end
)

(define a ((c-lambda () int "___result = a;")))
(define b ((c-lambda () unsigned-int "___result = b;")))

(println "a is" (if (eqv? a b) "" " not") " equal to b.")

(println "a does" (case a ((b) "") (else " not")) " match b in case.")
