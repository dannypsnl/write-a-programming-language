# Semantic Checker

Semantic checking can be a super complicate issue, but rather than push you into the crazy formal world, I would only show a simple overview about semantic checking. Semantic checking, would usually get confused with the concept about type checking, but not. Semantic checking includes type checking. For example, we can say expression `1/0` is invalid, whatever in runtime or compile time(CPU handles this usually by the way, also can see as a runtime checking). Semantic checking can do much more in runtime, but find out troubles in compile time as possible make a more robust system. A type, can be considered as properties of an object, with checking we can avoid unexpected input, e.g. `(lambda ([x : Integer]) x)` never takes `"Hello"` as the parameter, compiler can find out this. Now we get the first example of type check: same type checking. What's the same can be much more complicate than you might expect, but here I focus on the concept of same type instance. In the example, `Integer` is a **builtin** type, which means there can have non-builtin type, for example in **C** we can have `struct XXX` or `typedef`. With these, we can get start with our first language:

```racket
;;; Types
(struct Integer ())
(struct Arrow (param-typ return-typ))
; in Racket, * usually stands for many
(struct Struct (name field*))
(struct Field (typ name))
;;; Terms
(struct Struct/value (struct-name term*))
(struct Int (value))
(struct Var (typ name))
(struct Func (var term))
(struct Func/call (term1 term2))
;;; BTW, add #:transparent for all structs
```

We can simpling assuming there has no syntax error since parser should handle this than use valid abstraction syntax tree. What we interest on is how to check an input tree is valid(`x : t` is commonly stands for type of `x` is `t`):

```racket
; t: tree
; ty: Type
; env: environment map
; return bool, #t for valid, #f for invalid
(define (: t ty env)
  (cond
    [(Int? t) (eqv? ty (Integer))]
    [(Var? t) (: (lookup/type-of env (Var-name t)) ty env)]
    [(Struct/value? t)
     (let ([typ (lookup/type-by-name env (Struct/value-struct-name t))])
       (andmap (lambda (field field-value)
                 ; each field value should be valid member of field type
                 (: field-value (Field-typ field) env))
               (Struct-field* typ)
               (Struct/value-term* t)))]
    [(Func? t)
     (let ([env (extend/env env (Var-name (Func-var t)) (Var-typ (Func-var t)))])
       (if (Arrow? ty)
         (and (eqv? (Var-typ (Func-var t)) (Arrow-param-typ ty))
              (: (Func-term t) (Arrow-return-typ ty) env))
         #f))]
    [(Func/call? t)
     (if (Func? (Func/call-term1 t))
       (: (Func/call-term2 t) ; argument term should have function var required type
              (Var-typ (Func-var (Func? (Func/call-term1 t))))
              env)
       ;;; although I should handle much more complicate example like (((lambda (x) (lambda (a) (+ a x))) 1) 2)
       ; but for simple, here just reject indirect function call
       #f)]))
```

As the last case shows, `:` is not good enough, the problem is we can have deeper function hiding in the tree. To find out them, we need unification: a function takes type, type, environment and returns a type. Obviously, we have to know type of sub-term, get type from a term called inference.

TODO: inference

## Lambda Cube

### lambda calculus

### Simply typed lambda calculus

### polymorphism

#### unification

#### let polymorphism

### dependent type

#### types depends on terms

### calculus of construction
