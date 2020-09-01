#lang racket

(define op* '(add sub mul div))

(define (inst->string inst)
  (match inst
    [`(load ,r ,e)
     (format "LOAD ~a, ~a" r e)]
    [`(store ,r ,e)
     (format "STORE ~a, ~a" r e)]
    [`(,op ,r0 ,r1 ,e)
     (unless (member op op*)
       (error 'unknown-op "invalid instruction: ~a" op))
     (format "ADD ~a, ~a, ~a" r0 r1 e)]
    ['(ret) "RET"]
    ['(halt) "HALT"]))

(for ([inst '((load r0 1)
              (add r0 r0 1)
              (store r1 r0)
              (halt))])
  (displayln (inst->string inst)))
