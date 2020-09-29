#lang typed/racket

(require/typed "basic-block.rkt"
               [#:struct basic-block
                ([inst* : (Listof inst)])]
               [basic-block-push! (-> basic-block inst Void)])

(define-type Oprend (U Symbol Integer Boolean))

(define-type Expr (U Oprend expr))
(struct expr
  ([a : Oprend]
   [b : Oprend]) #:transparent)
(struct + expr () #:transparent)
(struct - expr () #:transparent)
(struct * expr () #:transparent)
(struct / expr () #:transparent)
(struct = expr () #:transparent)
(struct > expr () #:transparent)
(struct < expr () #:transparent)
(struct >= expr () #:transparent)
(struct <= expr () #:transparent)

(struct inst () #:transparent)
(struct assign inst
  ([var : Symbol]
   [exp : Expr]) #:transparent)
(struct jump inst
  ([label : Symbol]
   [cond : Expr]) #:transparent)
(struct label inst
  ([name : Symbol]) #:transparent)

(: inst*->leader* (-> (Listof inst) (Listof Boolean)))
(define (inst*->leader* inst*)
  (define jump-target* : (HashTable Symbol Boolean)
    (make-hash))
  (for ([inst inst*])
    (match inst
      [(jump t cond) (hash-set! jump-target* t #t)]
      [else (void)]))
  (map (λ ([i : inst])
         (match i
           [(label name)
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
  (list (assign 'r0 1)
        (label 'bar)
        (assign 'r0 (+ 'r0 1))
        (assign 'r1 'r0)
        (jump 'foo #t)
        (label 'foo)
        (jump 'bar (= 'r1 0))))

(inst*->basic-block* test-prog)
