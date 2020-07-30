# Semantic Checker

Semantic checking can be a super complicate issue, but rather than push you into the crazy formal world, I would only show a simple overview about semantic checking. Semantic checking, would usually get confused with the concept about type checking, but not. Semantic checking includes type checking. For example, we can say expression `1/0` is invalid, whatever in runtime or compile time(CPU handles this usually by the way, also can see as a runtime checking). Semantic checking can do much more in runtime, but find out troubles in compile time as possible make a more robust system. A type, can be considered as properties of an object, with checking we can avoid unexpected input, e.g. `(lambda ([x : Integer]) x)` never takes `"Hello"` as the parameter, compiler can find out this. Now we get the first example of type check: same type checking. What's the same can be much more complicate than you might expect, but here I focus on the concept of same type instance. In the example, `Integer` is a **builtin** type, which means there can have non-builtin type, for example in **C** we can have `struct XXX` or `typedef`. With these, we can get start with our first language:

```rkt
;;; Terms
(struct Struct/value (struct-name term*) #:transparent)
(struct Int (value) #:transparent)
(struct Var (typ name) #:transparent)
(struct Func (var term) #:transparent)
(struct Func/call (term1 term2) #:transparent)
```

We can simpling assuming there has no syntax error since parser should handle this than use valid abstraction syntax tree. What we interest on is how to check an input tree is valid(`x : t` is commonly stands for type of `x` is `t`):

```rkt
; t: tree
; ty: Type
; env: environment map
; return bool, #t for valid, #f for invalid
(define (: t ty [env (make-immutable-hash '())])
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
```

Before keep going, you can solve these questions: What's `env` and create `lookup/type-by-name`, `lookup/type-of`, and `extend/env` to make `:` works.

As the last case shows, `:` is not good enough, the problem is we can have deeper function hiding in the tree. To find out them, we need inference: a function get type from a term. Then check inferred result and expected type are the same thing.

```rkt
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
```

With `infer` we can improve `:` and make it work with sub-term with function(lambda)!

```diff
-     (if (Func? (Func/call-term1 t))
-         (: (Func/call-term2 t) ; argument term should have function var required type
-            (Var-typ (Func-var (Func? (Func/call-term1 t))))
-            env)
-         ;;; although I should handle much more complicate example like (((lambda (x) (lambda (a) (+ a x))) 1) 2)
-         ; but for simple, here just reject indirect function call
-         #f)]))
+     (let ([t1 (Func/call-term1 t)]
+           [t2 (Func/call-term2 t)])
+       (match (infer t1 env)
+         [`(-> ,param-typ ,return-typ)
+          (: t2 param-typ env)]
+         [else #f]))]))
```

Now we get a simple language have lambda/function, and some builtin types, however, sometimes we want higher type: a type depends on type, e.g. `(list int)`(or `list<int>`). To define such type constructor we need to leave type hole/parameters, we call such type constructor **polymorphism**. The most famous system call Hindley-Milner system which has strong typing without any type annotation, although now we know such ability is not that important even harmful, we learn it for the concept of unification:

```rkt
(define (occurs v t)
  (match* (v t)
    [(v `(,t* ...))
     (ormap (λ (t) (occurs v t)) t*)]
    ((v t) (equal? v t))))

(define (unify t1 t2)
  (match* (t1 t2)
    [(_ t2) #:when (parameter? t2)
            (if (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
                (t2 t1)
                (error (format "~a occurs in ~a" (t2) t1)))]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (for-each unify a* b*)]
    [(_ _)
     (unless (eqv? t1 t2)
       (error (format "cannot unify type ~a and ~a" t1 t2)))]))

;;; infer should take a term and produce a type
(define (recur-infer tm [env (make-immutable-hash)])
  (match tm
    [`(λ (,x* ...) ,t)
     (let ([λ-env (foldl (λ (x e)
                           (extend/env e x (make-parameter (gensym))))
                         env x*)])
       `(-> ,(map (λ (x) (lookup/type-of λ-env x)) x*)
            ,(recur-infer t λ-env)))]
    [`(let ([,x* ,xt*] ...) ,t)
     (let ([let-env (foldl (λ (x t e)
                             (extend/env e x (recur-infer t e)))
                           env x* xt*)])
       (recur-infer t let-env))]
    [`(quote ,p*)
     `(list ,(if (empty? p*)
                 (make-parameter (gensym))
                 (let ([et (recur-infer (car p*) env)])
                   (for-each (λ (et*) (unify et* et))
                             (map (λ (x) (recur-infer x env)) (cdr p*)))
                   et)))]
    [`(,f ,arg* ...)
     (let ([free (make-parameter (gensym))])
       (unify (recur-infer f env)
              `(-> ,(map (λ (arg) (recur-infer arg env)) arg*) ,free))
       free)]
    [x (cond
         [(string? x) 'string]
         [(number? x) 'number]
         [(char? x) 'char]
         [(symbol? x) (lookup/type-of env x)]
         [else (error (format "unknown form: ~a" x))])]))

(define (elim-free ty)
  (match ty
    [`(,ty* ...)
     (map elim-free ty*)]
    [ty (if (parameter? ty)
            (elim-free (ty))
            ty)]))

(define (infer tm) (elim-free (recur-infer tm)))
```

TODO: polymorphism
TODO: unification
TODO: let polymorphism
TODO: dependent type
