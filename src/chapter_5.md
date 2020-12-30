# Complicated Inference

A problem would come when we move into more complicate type system: can we avoid type mark but still get benefit from type system? The answer is no for **polymorphic lambda calculus(also known as system F)**, for example

```racket
(λ (f) (f 1))
```

we know `1` has type `Number`, but what's the result type of `f`? At most, we can get `f : Number -> ?`, but nothing more. Thus, the inference on **system F** is not decidable. However, the following code would work:

```racket
((λ (f) (f 1)) (λ (x) x))
```

We get `f : a -> a` from `(λ (x) x)`, then `a = Number` when apply `f` with `1`, hence `((λ (f) (f 1)) (λ (x) x)) : Number`. The most important observe here is: if we can detect the implementation of a binding, then inference is decidable. Thus, **Hindley-Milner type system** introduces **let binding** into the system, which gives a such form.

TODO
