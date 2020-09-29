#lang racket

(provide (struct-out basic-block)
         basic-block-push!)

(struct basic-block
  (inst*)
  #:methods gen:custom-write
  [(define (write-proc bb port mode)
     (fprintf port "basic block:~n")
     (for ([inst (basic-block-inst* bb)])
       (fprintf port "\t~a~n" inst)))]
  #:mutable
  #:transparent)

(define (basic-block-push! bb inst)
  (set-basic-block-inst*! bb (append (basic-block-inst* bb) (list inst))))
