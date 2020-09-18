#lang typed/racket

(provide (all-defined-out))

(define-type reg (U 'r0 'r1))

(define-type expr (U reg Integer))

(struct inst () #:transparent)
(struct load inst
  ([r : reg]
   [e : expr]) #:transparent)
(struct store inst
  ([r : reg]
   [e : expr]) #:transparent)
(struct tag inst
  ([name : Symbol]) #:transparent)
(struct brz inst
  ([r : reg]
   [t : Symbol]) #:transparent)
(struct br inst
  ([t : Symbol]) #:transparent)
(struct op inst
  ([op : (U 'add 'sub 'mul 'div)]
   [r0 : reg]
   [r1 : reg]
   [e : expr]) #:transparent)
(struct ret inst () #:transparent)
(struct halt inst () #:transparent)

(: inst->string (-> inst String))
(define (inst->string inst)
  (match inst
    [(load r e)
     (format "LOAD ~a, ~a" r e)]
    [(store r e)
     (format "STORE ~a, ~a" r e)]
    [(tag name)
     (format "~a:" name)]
    ; jump if r contains zero
    [(brz r t)
     (format "BRZ ~a ~a" r t)]
    ; always jump
    [(br t)
     (format "BR ~a" t)]
    [(op op r0 r1 e)
     (format "~a ~a, ~a, ~a" op r0 r1 e)]
    [(ret) "RET"]
    [(halt) "HALT"]))
