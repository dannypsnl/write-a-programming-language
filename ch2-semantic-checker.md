# Semantic Checker

Semantic checking can be a super complicate issue, but rather than push you into the crazy formal world, I would only show a simple overview about semantic checking. Semantic checking, would usually get confused with the concept about type checking, but not. Semantic checking includes type checking. For example, we can say expression `1/0` is invalid, whatever in runtime or compile time(CPU handles this usually by the way, also can see as a runtime checking). Semantic checking can do much more in runtime, but find out troubles in compile time as possible make a more robust system. A type, can be considered as properties of an object, with checking we can avoid unexpected input, e.g. `(lambda ([x : Integer]) x)` never takes `"Hello"` as the parameter, compiler know `"Hello"` is not an `Integer`. This is the first example of type check: same type checking. What's the same can be much more complicate than you might expect, but here I focus on the concept of **same type instance**. In this example, `Integer` is a **builtin** type, its equal rule only allows exactly same type representation: `Integer`. With builtin, of course there have non-builtin types, for example in **C** we can use `struct XXX` or `typedef` to create new type. With these basic concepts, we can get start with our first language.

### For Fun: Dependent type

In the end, for fun I would make a toy dependent type as the final part of this chapter, which means type can depend on term(value), or type is just a term. Under this perspective, we can have some interesting definitions(**Agda**):

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

1. Assuming all term was well-typed, this can be ensured by only using `tt` to ensure.

   ```rkt
   (struct tt (tm ty)
     #:methods gen:custom-write
     [(define (write-proc tt port mode)
        (fprintf port "~a" (pretty tt)))]
     #:transparent)
   ```

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
(sym)

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
(+0/Nat)
```

Notice the definition of `match` can only work for such simple case, it won't work with `? : Nat`, also lacking termination check which cannot be a safe definition. To get more information about termination, can search **recursor** or **guard predicate**(used by **Coq**).
