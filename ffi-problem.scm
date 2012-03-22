(c-declare #<<c-declare-end

typedef struct node node; 
struct node {int x; node* next;};

c-declare-end
)

(c-define-type node (struct "node"))
(c-define-type node* (pointer "node"))

(define make-node 
  (c-lambda () node 
    "___result_voidstar = ___EXT(___alloc_rc)(sizeof(node));"))

(define node-pointer
  (c-lambda (node) (pointer node)
    "___result_voidstar = ___arg1_voidstar;"))

(define pointer->node
  (c-lambda ((pointer node)) node
    "___result_voidstar = ___arg1_voidstar;"))

(define node-x-set!
  (c-lambda (node int) void
    "(*(node*)___arg1_voidstar).x = ___arg2;"))

(define node-x
  (c-lambda (node) int
    "___result = ((node*)___arg1_voidstar)->x;"))

(define node-next
  (c-lambda (node) (pointer node)
    "___result_voidstar = ((node*)___arg1_voidstar)->next;"))

(define node-next-set! 
  (c-lambda (node node*) void 
    "(*(node*)___arg1_voidstar).next = (node*)___arg2_voidstar;"))

(define n1 (make-node))
(define n2 (make-node))
(node-x-set! n2 5)
(node-next-set! n1 (node-pointer n2))
(println (node-x (pointer->node (node-next n1))))

