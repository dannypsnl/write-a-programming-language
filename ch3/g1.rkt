#lang typed/racket

(require "instruction.rkt")
(require/typed "basic-block.rkt"
               [#:struct basic-block
                ([inst* : (Listof inst)])]
               [basic-block-push! (-> basic-block inst Void)])

(: inst*->leader* (-> (Listof inst) (Listof Boolean)))
(define (inst*->leader* inst*)
  (define jump-target* : (HashTable Symbol Boolean)
    (make-hash))
  (for ([inst inst*])
    (match inst
      [(brz r t) (hash-set! jump-target* t #t)]
      [(br t) (hash-set! jump-target* t #t)]
      [else (void)]))
  (map (λ ([i : inst])
         (match i
           [(tag name)
            (hash-ref jump-target* name #f)]
           [else #f]))
       (cdr inst*)))

(: inst*->basic-block* (-> (Listof inst) (Listof basic-block)))
(define (inst*->basic-block* inst*)
  (define block* : (Listof basic-block)
    '())
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

(define test-prog : (Listof inst)
  (list (load 'r0 1)
        (tag 'bar)
        (op 'add 'r0 'r0 1)
        (store 'r1 'r0)
        (br 'foo)
        (tag 'foo)
        (brz 'r1 'bar)
        (halt)))

(inst*->basic-block* test-prog)
