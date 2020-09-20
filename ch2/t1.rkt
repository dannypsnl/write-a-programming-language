#lang racket

;;; Terms
(struct Struct/value (struct-name term*) #:transparent)
(struct Int (value) #:transparent)
(struct Var (typ name) #:transparent)
(struct Func (var term) #:transparent)
(struct Func/call (term1 term2) #:transparent)

(define (lookup/type-of env v)
  (hash-ref env v))
(define (extend/env env v t)
  (hash-set env v t))

(define (infer t [env (make-immutable-hash)])
  (cond
    [(Int? t) 'Integer]
    [(Var? t) (lookup/type-of env (Var-name t))]
    [(Struct/value? t)
     `(struct
        ,(Struct/value-struct-name t)
        ,(map (λ (field)
                (infer field env))
              (Struct/value-term* t)))]
    [(Func? t)
     ;;; introduce parameter into environment
     (let ([env (extend/env env (Var-name (Func-var t)) (Var-typ (Func-var t)))])
       `(-> ,(Var-typ (Func-var t)) ,(infer (Func-term t) env)))]
    [(Func/call? t)
     (match (infer (Func/call-term1 t) env)
       [`(-> ,param-typ ,return-typ)
        (if (eqv? (infer (Func/call-term2 t) env) param-typ)
            return-typ
            (error "type mismatch"))]
       [_ (error "call on non-arrow type")])]))

; t: tree
; ty: Type
; env: environment map
; return bool, #t for valid, #f for invalid
(define (: t ty [env (make-immutable-hash)])
  (equal? (infer t env) ty))

(: (Int 1) 'Integer)
(: (Func (Var 'Integer 'x) (Var 'Integer 'x)) '(-> Integer Integer))

(infer (Func (Var 'Integer 'x) (Var 'Integer 'x)))
