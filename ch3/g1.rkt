#lang racket

(define op* '(add sub mul div))

(define (inst->string inst)
  (match inst
    [`(load ,r ,e)
     (format "LOAD ~a, ~a" r e)]
    [`(store ,r ,e)
     (format "STORE ~a, ~a" r e)]
    [`(tag ,name)
     (format "~a:" name)]
    ; jump if r contains zero
    [`(brz ,r ,t)
     (format "BRZ ~a ~a" r t)]
    ; always jump
    [`(br ,t)
     (format "BR ~a" t)]
    [`(,op ,r0 ,r1 ,e)
     (unless (member op op*)
       (error 'unknown-op "invalid instruction: ~a" op))
     (format "ADD ~a, ~a, ~a" r0 r1 e)]
    ['(ret) "RET"]
    ['(halt) "HALT"]))

(define (inst*->leader* inst*)
  (define jump-target* (make-hash))
  (for ([inst inst*])
    (match inst
      [`(brz ,r ,t) (hash-set! jump-target* t #t)]
      [`(br ,t) (hash-set! jump-target* t #t)]
      [else (void)]))
  (map (λ (i)
         (match i
           [`(tag ,name)
            (hash-ref jump-target* name #f)]
           [else #f]))
       (cdr inst*)))

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

(define (inst*->basic-block* inst*)
  (define block* '())
  (define cur-block (basic-block (list (car inst*))))
  (for ([inst (cdr inst*)]
        [leader? (inst*->leader* inst*)])
    (if leader?
        (begin
          (set! block* (append block* (list cur-block)))
          (set! cur-block (basic-block '()))
          (basic-block-push! cur-block inst))
        (basic-block-push! cur-block inst)))
  (append block* (list cur-block)))

(define test-prog
  '((load r0 1)
    (tag bar)
    (add r0 r0 1)
    (store r1 r0)
    (br foo)
    (tag foo)
    (brz r1 bar)
    (halt)))

(inst*->basic-block* test-prog)
