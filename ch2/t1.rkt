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

; t: tree
; ty: Type
; env: environment map
; return bool, #t for valid, #f for invalid
(define (: t ty [env (make-immutable-hash)])
  (cond
    [(Int? t) (eq? ty 'Integer)]
    [(Var? t) (eq? (lookup/type-of env (Var-name t)) ty)]
    [(Struct/value? t)
     (match ty
       [`(struct ,name ,field-typ*)
        (and (string=? name (Struct/value-struct-name t))
             (andmap (λ (field-typ field-value)
                       ; each field value should be valid member of field type
                       (: field-value field-typ env))
                     field-typ*
                     (Struct/value-term* t)))]
       [else #f])]
    [(Func? t)
     (let* ([v (Func-var t)]
            [v-name (Var-name v)]
            [v-typ (Var-typ v)])
       (match ty
         [`(-> ,param-typ ,return-typ)
          (and (eqv? v-typ param-typ)
               (: (Func-term t) return-typ (extend/env env v-name v-typ)))]
         [else #f]))]
    [(Func/call? t)
     (if (Func? (Func/call-term1 t))
         (: (Func/call-term2 t) ; argument term should have function var required type
            (Var-typ (Func-var (Func? (Func/call-term1 t))))
            env)
         ;;; although I should handle much more complicate example like (((lambda (x) (lambda (a) (+ a x))) 1) 2)
         ; but for simple, here just reject indirect function call
         #f)]))

(: (Int 1) 'Integer)
(: (Func (Var 'Integer 'x) (Var 'Integer 'x)) '(-> Integer Integer))

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

(infer (Func (Var 'Integer 'x) (Var 'Integer 'x)))
