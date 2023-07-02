= Chapter 7: Dependent Type

Lambda Cube demonstrate an interesting picture about type theory world, but it just a start, **Calculus of Construction** needs to consider reality to work. We can imagine a function like the following.

```rkt
(define (A-or-B [b : Bool] [A B : Type]) : Type
  (match b
    [true => A]
    [false => B]))
```

Obviously,

- `(A-or-B true Nat Bool)` produces `Nat`
- `(A-or-B false Nat Bool)` produces `Bool`

Thus, we can have definition

```rkt
(define a : (A-or-B true Nat Bool)
  1)
```

which very make sense.

Until we found something the following.

```rkt
(define (endless [n : Nat]) : Type
  (match n
    [zero => Bool]
    [(suc ,n) => (endless (suc n))]))
```

- `(endless zero)` produces `Bool`
- However, `(endless (suc n))` for any `n : Nat` produces nothing!

The computation even won't get stop! Then our type checking just a joke since halting problem is undecidable.

Thus, we need termination check.

== Termination Check

The simplest solution was ensuring program could be converted to eliminator, the only weak point was this approach couldn't solve more complicated case but only primitive pattern(only expand one level for all constructors of an inductive type). For example

```rkt
(define (+ [n m : Nat]) : Nat
  (match n
    [zero => m]
    [(suc ,n) => (suc (+ n m))]))
```

Another approach is ensuring no greater construction in expression of pattern by syntax checking(I remember this one is the first showed approach). For example, `(suc (suc n))` would get rejected with pattern `(suc ,n)`.

A more recently solution was Mini-Agda's sized type, this is associating a constructor with a number and some arithmetic rules to judge which is greater construction. For example, if `(suc ,n)` has size `k`(a number), then `n` has size `k-1`, therefore, `(suc (suc n))` has `k+1` is greater than origin pattern. This approach can even handle cross function tracing.
