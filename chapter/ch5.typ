= Chapter 5: Inference code with polymorphic type


You already learn type inference concept, but if you keep going, you will find the code in chapter 2 cannot handle many important case. With polymorphism, our type getting more horrible now. Can we avoid most type mark and still get benefit from the type system? The answer is no for **polymorphic lambda calculus(also known as system F)**, for example, the following code cannot have fixed result

```haskell
\f -> f 1
```

Let's see how it goes to 

we know `1` has type `Integer`, but what's the result type of `f`? It can be `f : Integer -> ?`, but if we cannot ensure `?` is what, we are not able to unify `?` with any type. Thus, the inference on **system F** is not decidable. However, the following code is type checked

```haskell
(\f -> f 1) (\x -> x)
```

We get `f : a -> a` from `\x -> x`, then `a = Integer` when apply `f` with `1`, hence `(\f -> f 1) (\x -> x) : Integer`. The most important observe here is: If we can detect the implementation of an appliable binding, then inference is decidable. Thus, **Hindley-Milner type system** introduces **let binding** into the system, which gives a such form.

== Implementation

The first part is about syntax `recur-infer` build type by traveling on the syntax tree, we give any unknown type a tag(free variable type) to keep who they're, such tag is encoding by a `procedure` for **GEN** and **INST** rule, and a `parameter` for **Free**. A binding must use **INST** rule to generate a clean new type instance, for example:

```haskell
let id = \x -> x
in (id 1, id "")
```

`id` must need to be instantiated before using, else we would see `error: cannot unify number and string` since `id` unify its free variable with `number` first, then unify with `string` but sharing a same instance.

```rkt
(define (recur-infer tm [env (make-immutable-hash)])
  (match tm
```

- Lambda rule is simple, a `(-> (parameter-type* ...) return-type)`, but we didn't know the type of parameter, therefore, given `?0`, `?1`, `?2` and so on. Then create a new environment to infer its body.

  ```rkt
      [`(λ (,x* ...) ,t)
       (let ([λ-env (foldl (λ (x e)
                             (extend/env e x (make-parameter (gensym '?))))
                           env x*)])
         `(-> ,(map (λ (x) (recur-infer x λ-env)) x*)
              ,(recur-infer t λ-env)))]
  ```

- let rule, which seems like not need, is quite important. In Racket, a possible transformation is `let` to `lambda`, however, in HM system they are different as we say above. However, notice that we can make a trick: **bind the inferred type to abstraction's parameter if it's an immediate application**. Another way is introducing make polymorphism type can in the definition of parameter.

  ```rkt
      [`(let ([,x* ,xt*] ...) ,t)
       (let ([let-env (foldl (λ (x t e)
                               (extend/env e x
                                           (λ () (recur-infer t e))))
                             env x* xt*)])
         (recur-infer t let-env))]
  ```

- `list` are something like `'(1 2 3)`, `pair` are `(pair 1 2)`. In these cases, we return `(<list or pair> ?)` if no elements, we will not sure what's `?`(use `(make-parameter (gensym))`) till we get some operations like: `(append a-list 1)` then infer `?` via application rule. If there have elements, we infer via first element, and check rest elements!

  ```rkt
      [`(pair ,a ,b)
       `(pair ,(recur-infer a env) ,(recur-infer b env))]
      [`(quote ,p*)
       `(list ,(if (empty? p*)
                   (make-parameter (gensym '?))
                   (let ([et (recur-infer (car p*) env)])
                     (for-each (λ (et*) (unify et* et))
                               (map (λ (x) (recur-infer x env)) (cdr p*)))
                     et)))]
  ```

- Application rule unify the `f` type with a new arrow(`->`) type which constructed by arguments' type, and a free type variable for return type. Then give final return type as its result.

  ```rkt
      [`(,f ,arg* ...)
       (let ([free (make-parameter (gensym '?))])
         (unify (recur-infer f env)
                `(-> ,(map (λ (arg) (recur-infer arg env)) arg*) ,free))
         free)]
  ```

- Finally, we get some simple type(monolithic)

  ```rkt
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
  ```

Above program separate and explain how the key part working, in the last step, once we get all result, we remove all free variable as possible.

```rkt
(define (elim-free ty)
  (match ty
    [`(,ty* ...)
     (map elim-free ty*)]
    [ty (if (parameter? ty)
            (elim-free (ty))
            ty)]))

(define (infer tm) (elim-free (recur-infer tm)))
```

Then here was the key of all the stuff: `occurs` and `unify`, **unification** is all about binding variable with any order. Thus, `(unify ?a int)` and `(unify int ?a)` should produce same result and make `?a` be `int`, and we also believe `?a` cannot unify with `string` again since its `int` and `int` is not `string`. However, unifying `?a` with `?b` is an option, it has no different with `(unify ?b int)` and `unify ?b ?a`. The last thing we need to be careful was recursion, consider if we `(unify ?a (list ?a))`, our process would run into trouble: `?a` is `(list ?a)`, but what's `(list ?a)`? We expand `?a` then get `(list (list ?a))`, but then what's `?a`? Again and again... Thus, we must check `?a` didn't occur in the type which it's going to bind.

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
```

== More

Though I said rebound is unacceptable, in fact we can make several variants of type systems on this, by introducing one of **union type**, **higher rank type**, **row polymorphism**, we can get lots of fun. **Hindley-Milner type system** is not a good system in practice what we already know, even **Haskell** best practice would tell you at least give the top level binding a type annotation. However, the core of **Hindley-Milner type system**, unification was really important in the more advanced type system like **Dependent type** since we need to deal with a lots of duplicate annotation in these variants.
