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
  ; first is leader
  (cons #t
        (map (λ (i)
               (match i
                 [`(tag ,name)
                  (hash-ref jump-target* name)]
                 [else #f]))
             (cdr inst*))))

(define test-prog
  '((load r0 1)
    (tag bar)
    (add r0 r0 1)
    (store r1 r0)
    (br foo)
    (tag foo)
    (brz r1 bar)
    (halt)))

(inst*->leader* test-prog)

(for ([inst test-prog])
  (displayln (inst->string inst)))
