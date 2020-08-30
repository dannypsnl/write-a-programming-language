# Semantic Checker

Semantic checking can be a super complicate issue, but rather than push you into the crazy formal world, I would only show a simple overview about semantic checking. Semantic checking, would usually get confused with the concept about type checking, but not. Semantic checking includes type checking. For example, we can say expression `1/0` is invalid, whatever in runtime or compile time(CPU handles this usually by the way, also can see as a runtime checking). Semantic checking can do much more in runtime, but find out troubles in compile time as possible make a more robust system. A type, can be considered as properties of an object, with checking we can avoid unexpected input, e.g. `(lambda ([x : Integer]) x)` never takes `"Hello"` as the parameter, compiler know `"Hello"` is not an `Integer`. This is the first example of type check: same type checking. What's the same can be much more complicate than you might expect, but here I focus on the concept of **same type instance**. In this example, `Integer` is a **builtin** type, its equal rule only allows exactly same type representation: `Integer`. With builtin, of course there have non-builtin types, for example in **C** we can use `struct XXX` or `typedef` to create new type. With these basic concepts, we can get start with our first language:

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
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    (t (equal? v t))))

(define (unify t1 t2)
  (match* (t1 t2)
    [(_ t2) #:when (and (parameter? t2)
                        ;;; ensure t2 is still free
                        (string-prefix? (symbol->string (t2)) "?"))
            (when (or (eqv? t1 (t2)) (occurs (t2) t1))
              (error (format "~a occurs in ~a" (t2) t1)))
            (t2 t1)]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (for-each unify a* b*)]
    [(_ _)
     (let ([a (elim-free t1)]
           [b (elim-free t2)])
       (unless (eqv? a b)
         (error (format "cannot unify type ~a and ~a" a b))))]))

(define (recur-infer tm [env (make-immutable-hash)])
  (match tm
    [`(λ (,x* ...) ,t)
     (let ([λ-env (foldl (λ (x e)
                           (extend/env e x (make-parameter (gensym '?))))
                         env x*)])
       `(-> ,(map (λ (x) (recur-infer x λ-env)) x*)
            ,(recur-infer t λ-env)))]
    [`(let ([,x* ,xt*] ...) ,t)
     (let ([let-env (foldl (λ (x t e)
                             (extend/env e x
                                         (λ () (recur-infer t e))))
                           env x* xt*)])
       (recur-infer t let-env))]
    [`(pair ,a ,b)
     `(pair ,(recur-infer a env) ,(recur-infer b env))]
    [`(quote ,p*)
     `(list ,(if (empty? p*)
                 (make-parameter (gensym '?))
                 (let ([et (recur-infer (car p*) env)])
                   (for-each (λ (et*) (unify et* et))
                             (map (λ (x) (recur-infer x env)) (cdr p*)))
                   et)))]
    [`(,f ,arg* ...)
     (let ([free (make-parameter (gensym '?))])
       (unify (recur-infer f env)
              `(-> ,(map (λ (arg) (recur-infer arg env)) arg*) ,free))
       free)]
    [x (cond
         [(string? x) 'string]
         [(number? x) 'number]
         [(char? x) 'char]
         [(symbol? x)
          (let ([t (lookup/type-of env x)])
            (if (and (procedure? t) (not (parameter? t)))
                (t)
                t))]
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

In this implementation, I simply pattern matching on **s-exp** to make code more compact, also use **s-exp** to represent type. The less pattern `x` takes all forms, then we use `cond` to handle different case:

```rkt
(infer "hello") ; string
(infer #\v) ; char
(infer 100) ; number
```

We only escape `symbol?`, this should lookup in the environment. Except these, are invalid form. Rest forms are `let`, `lambda(function)`, `list`, and `application(function call)`.

However, in symbol we not only lookup, that is because let rule would introduce **GEN**(represent by `(λ () t)`), therefore, if we get a procedure is not a parameter, it needs to be instantiated(rule **INST**). For example:

```rkt
(let ([id (λ (x) x)])
  (pair (id 1) (id "")))
```

In this case, `id` type should be instantiated before usage, else result type would be we cannot unify `number` and `string` caused by rebinding the free type variable which belongs to the parameter of `id` from `number` to `string`. With an indirect type produce from procedure, `id` get new free type variable in the second application, then we can get `(pair number string)` as expected.

`list` are something like `'(1 2 3)`, `pair` are `(pair 1 2)`. In these cases, we return `(<list or pair> ?)` if no elements, we will not sure what's `?`(use `(make-parameter (gensym))`) till we get some operations like: `(append a-list 1)` then infer `?` via application rule. If there have elements, we infer via first element, and check rest elements!

Lambda rule is simple, a `(-> (parameter-type* ...) return-type)`, but we didn't know the type of parameter, therefore, given `?0`, `?1`, `?2` and so on. Then use new envionment to infer its body.

Application rule unify the `f` type with a new arrow(`->`) type which constructed by arguments' type, and a free type variable for return type. Then give final return type as its result.

Finally, let rule, which seems like not need, is quite important. In Racket, a possible transformation is `let` to `lambda`, however, in HM system they are different, an example: `((λ (id) (id "")) (λ (x) x))`, we all know the answer is `string`, but it cannot produce this caused by `id` has a free type. With `let` rule, we can have: `(let ([id (λ (x) x)]) (id ""))` and get `string` as expected, but notice that, we can make a trick: bind the inferred type to abstraction's parameter if it's an immediate application. Another way is introducing make polymorphism type can in the definition of parameter.

### Dependent type

Dependent type is the final part of this chapter, which means type can depend on term(value), or type is just a term. Under this perspective, we can have some interesting definitions(**Agda**):

```agda
data Vec (A : Set a) : ℕ → Set a where
  []  : Vec A zero
  _∷_ : ∀ {n} (x : A) (xs : Vec A n) → Vec A (suc n)
```

We can try to represent these with **S expression**

```rkt
(ind Nat
  (z Nat)
  (s (-> Nat Nat)))
(ind (Vec [a Type] [n Nat])
  (vecnil (Vec a z))
  (vec:: (-> a (Vec a n) (Vec a (s n)))))
```

Where `(vec:: z vecnil)` is a evidence of type `(Vec Nat (s z))`. A simple partial interpreter of this example can be created in a few step by removing some high-level abilities from language.

1. Assuming all term was well-typed, this can be ensured by only using `(struct tt (tm ty) #:transparent)` to ensure.
2. The type of type is `U`, the type of `U` is `U`. `U` stands for the universe, but you can ignore it currently.
3. Without syntax sugar.
4. No optimizing.
5. Remove some checking(which means language is not as safe as language like **Agda** or any other theorem proof assitant).

Then we can start creating our first dependent type language under **Racket**. For every `inductive` data type, we use several `define` to define it.

```rkt
(define Bool (tt 'Bool U))
(define (true) (tt 'true Bool))
(define (false) (tt 'false Bool))

(define Nat (tt 'Nat U))
(define (z) (tt 'z Nat))
(define (s n)
  (: n Nat)
  (tt `(s ,n) Nat))
```

`Bool` and `Nat` are quite easy to understand, helper functions we need at here were `tt`(defined) and `:`, follow typing rule, `a : A` means a judgement that `a` has type `A` and represent as `(: a A)` in our language. Implementation of `:` is take out type of term(we ensure every terms were well-typed under `tt`) and ensure it's same as expected one.

```rkt
(define (: term type)
  (unless (ty= (tt-ty term) type)
    (error (format "~a is a ~a, not a ~a"
                   (pretty term)
                   (pretty (tt-ty term))
                   (pretty type)))))
```

Let's ignore `pretty` for now, and dig into another helper: `ty=`, `ty=` stands for type equality checking, and simply based on `equal?` test since we would only use **S expression** to represent type.

```rkt
(define (ty= t1 t2)
  (unless (equal? (pretty t1) (pretty t2))
    (error (format "~a != ~a" (pretty t1) (pretty t2)))))
```

Since all term was well-typed, inference can be simple.

```rkt
(define (<- t)
  (match t
    ['U U]
    [t (tt-ty (?/get t))]))
```

`?/get` is a helper, in this language, sometime we have free variables. For example: `(nil)` is a `(List ?)`, we have no idea what was `?` yet. To represent such a concept, we prepared `?` and `?/get`.

```rkt
(define (? ty) (make-parameter (tt (gensym '?) ty)))
(define (?/get p?) (if (parameter? p?) (p?) p?))
```

But these were helper, the core concept was `unify`, `unify` works for code like: `(:: (z) (nil))`. Its type should be `(List Nat)`, the way was `unify (List ?nil) (List Nat)`.

```rkt
(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    [t (equal? v t)]))
(define (unify t1 t2)
  (match* (t1 t2)
    [(_ (? parameter?))
     (unless (or (eqv? t1 (?/get t2)) (not (occurs (?/get t2) t1)))
       (error (format "~a occurs in ~a" (?/get t2) (?/get t1))))
     (t2 (?/get t1))]
    ; swap
    [((? parameter?) _) (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (map unify a* b*)]
    [((tt tm1 ty1) (tt tm2 ty2))
     (unify ty1 ty2)
     (unify tm1 tm2)]
    ; not free variable, then we expect they are the same
    [(_ _) (ty= t1 t2)]))
```

All need to do was keep checking any cases, and instantiate all free variable, final judge they are same. Occurs check prevents cycle reference in type/term. The final function was `pretty`, which prints term better.

```rkt
(define (pretty t)
  (match (?/get t)
    [`(,a* ...) (map pretty a*)]
    [(tt tm ty) `(: ,(pretty tm) ,(pretty ty))]
    [t t]))
```

Finally, let's view some examples can work with these fundamental:

```rkt
(define (List A)
  (: A U)
  (tt `(List ,A) U))
(define (nil) (tt 'nil (List (? U))))
(define (:: #:A [A (? U)] a lst)
  (unify A (<- a))
  (: A U)
  (unify (List A) (<- lst))
  (tt `(:: ,a ,lst) (List A)))

(define (Vec LEN E)
  (: LEN Nat)
  (: E U)
  (tt `(Vec ,LEN ,E) U))
(define (vecnil) (tt 'vecnil (Vec (z) (? U))))
(define (vec:: #:E [E (? U)] #:LEN [LEN (? Nat)] e v)
  (unify E (<- e))
  (: E U)
  (unify (Vec LEN E) (<- v))
  (tt `(vec:: ,e ,v) (Vec (s LEN) E)))

(define (vec/length v)
    (define LEN (? Nat))
    (define E (? U))
    (unify (Vec LEN E) (<- v))
    LEN)
```

We can even make some proof!

```rkt
(define (≡ #:A [A (? U)] a b)
  (: A U)
  (unify (<- a) A)
  (: b A)
  (tt `(≡ ,A ,a ,b) U))
(define (refl #:A [A (? U)] #:a [a (? A)])
  (tt 'refl (≡ a a)))


(define (sym #:A [A (? U)] #:x [x (? A)] #:y [y (? A)]
             [P1 (? (≡ x y))])
  (unify (refl) P1)
  (let ([r (refl)])
    (unify (≡ y x) (<- r))
    r))
(pretty (sym))

(define (Nat/+ m n)
  (: m Nat)
  (: n Nat)
  (match (tt-tm (?/get m))
    ['z n]
    [`(s ,m-)
     (s (Nat/+ m- n))]))
(define (+0/Nat #:x [x (? Nat)])
  (let ([r (refl)])
    (unify (≡ (Nat/+ (z) x) x) (<- r))
    r))
(pretty (+0/Nat))
```

Notice the definition of `match` can only work for such simple case, it won't work with `? : Nat`, also lacking termination check which cannot be a safe definition.
