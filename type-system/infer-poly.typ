== Inference with polymorphic type

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

=== Implementation

The first part is about syntax `recur-infer` build type by traveling on the syntax tree, we give any unknown type a tag(free variable type) to keep who they're, such tag is encoding by a `procedure` for **GEN** and **INST** rule, and a `parameter` for **Free**. A binding must use **INST** rule to generate a clean new type instance, for example:

```haskell
let id = \x -> x
in (id 1, id "")
```

`id` must need to be instantiated before using, else we would see `error: cannot unify number and string` since `id` unify its free variable with `number` first, then unify with `string` but sharing a same instance.

```lean
def infer (ctx : Ctx) : Term -> M Typ
```

`M` is the assumed monad that complete all stateful changes we need, it usually is defined as abbreviation like below, but we will not dig into that.

```lean
abbrev M := StateT ...
```

- Lambda rule is simple, since it's type must be a arrow type form, `parameter-type -> return-type`, but we didn't know the type of parameter, therefore, given a type variable `?0`. Then create a new environment to infer its body.

  ```lean
    | .lam x t => do
      let synth_ty <- mkTyVar
      let lam_ctx = extend ctx x synth_ty
      return arrTy synth_ty (<- infer lam_ctx t)
  ```

- `let` rule, which seems like not need, is quite important. In Racket, a possible transformation is `let` to `lambda`, however, in HM system they are different as we say above. However, notice that we can make a trick: _bind the inferred type to abstraction's parameter if it's an immediate application_. Another way is introducing make polymorphism type can in the definition of parameter.

  ```lean
    | .let x t e => do
      let let_ctx <- extend ctx x (infer ctx t)
      infer let_ctx e
  ```

- `list` are something like `[1 2 3]`, `pair` are cartesian product `(1, 2)`. To infer type of list, must considering the case that we have no element in it `[]`. When handling such case, we still need to give it a type, this is another place we would like to insert a type variable. Thus, the first step is create a type variable, then unify each given elements with the variable, return the list type of the variable in the end. Pair is much more simple, we infer sub-expression and build a product type as result.

  ```lean
    | .pair a b => do
      return prodTy (<- infer ctx a) (<- infer ctx b)
    | .list xs => do
      let synth_ty <- mkTyVar
      for x in xs do
        unify (<- infer ctx x) synth_ty
      return listTy synth_ty
  ```

- Application rule unify the `f` type's domain with argument's type, if it didn't throw, then we get the codomain as result.

  ```lean
    | .app f arg => do
      let ft <- infer ctx f
      match ft with
        | .arrTy dom cod =>
          unify dom (infer ctx arg)
          return cod
        | _ => raise "not an appliable"
  ```

- Finally, we get some simple, builtin type(monolithic)

  ```rkt
    | .str _ => return strTy
    | .num _ => return numTy
    | .char _ => return charTy
    | .var x => return (lookup ctx x)
  ```

Above program separate and explain how the key part working. Then here was the key of all the stuff: `unify`.

*unification* is all about binding variable with any order. Thus, `unify ?a int` and `unify int ?a` should produce same result and make `?a` be `int`

We also believe `?a` cannot unify with `string` again since its `int` and `int` is not `string`, so below code must fail
```lean
unify ?a int
unify ?a string
```
However, unifying `?a` with `?b` is an option, it should have no different with `unify ?b int` and `unify ?b ?a`.

The last thing we need to be careful was recursion, consider if we `unify ?a (list ?a)`, our process would run into trouble: `?a` is `list ?a`, but what's `list ?a`? We have to expand `?a` then get `list (list ?a)`, but then what's `?a`? Again and again... Thus, we must check `?a` didn't occur in the type which it's going to bind.

Before we look into the code, we need a helper funciton `force`
```lean
def force : Typ -> M Typ
  | .tyVar x => do
    match <- solution x with
      | .none => return .tyVar x
      | .some ty => force ty
  | t => return t
```
This function will help us using solution rather then complex meta to do unification if there has a solution.

Then the occurs check
```lean
def occurs (m : Meta) (t : Typ) : M Bool := do
  match t with
    | .tyVar x => return x == m
    | .arrTy dom cod => do
      occurs m dom <|> occurs m cod
    | .prodTy a b => do
      occurs m a <|> occurs m b
    | .listTy a => do
      occurs m a
    | _ => return false
```

With all requirement is ready, we can write the `unify` function
```lean
def unify : Typ -> Typ -> M Bool
  | ty, .tyVar x | .tyVar x, ty => do
    if occurs x ty then
      throw "occurs check failed"
    else solve x ty
  | .arrTy dom cod, .arrTy dom' cod' => do
    unify dom dom'; unify cod cod'
  | .prodTy a b, .prodTy a' b' => do
    unify a a'; unify b b'
  | .listTy a, .listTy a' => do
    unify a a'
  | .strTy, .strTy => return true
  | .numTy, .numTy => return true
  | .charTy, .charTy => return true
  | .varTy x, .varTy y => return x == y
  | _, _ => throw "cannot unify"
```

=== More

Though I said rebound is unacceptable, in fact we can make several variants of type systems on this, by introducing one of *union type*, *higher rank type*, *row polymorphism*, we can get lots of fun. *Hindley-Milner type system* is not a good system in practice what we already know, even *Haskell* best practice would tell you at least give the top level binding a type annotation. However, the core of *Hindley-Milner type system*, unification was really important in the more advanced type system like *Dependent type* since we need to deal with a lots of duplicate annotation in these variants.
