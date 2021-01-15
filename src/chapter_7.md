# Dependent Type

Lambda Cube demonstrate an interesting picture about type theory world, but it just a start, **Calculus of Construction** needs to consider reality to work. We can imagine a function like the following.

```racket
(define (A-or-B [b : Bool] [A B : Type]) : Type
  (match b
    [true => A]
    [false => B]))
```

Obviously,

- `(A-or-B true Nat Bool)` produces `Nat`
- `(A-or-B false Nat Bool)` produces `Bool`

Thus, we can have definition

```racket
(define a : (A-or-B true Nat Bool)
  1)
```

which very make sense.

Until we found something the following.

```racket
(define (endless [n : Nat]) : Type
  (match n
    [zero => Bool]
    [(suc ,n) => (endless (suc n))]))
```

- `(endless zero)` produces `Bool`
- However, `(endless (suc n))` for any `n : Nat` produces nothing!

The computation even won't get stop! Then our type checking just a joke since halting problem is undecidable.
