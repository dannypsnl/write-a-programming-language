= Chapter 6: Lambda Cube

Lambda Cube, as its name, was a cube. The vertex of cube was type system, each edge shows a change to type system as a direction, and take a type system as source to produce another type system as target.

Its starts from **STLC**, we have three directions can pick.

1. polymorphism: $lambda 2$

    Also known as *System F*, this allows terms to depend on types, by the following rule.

    $
    frac(
      Gamma tack.r t : B,
      Gamma tack.r 
        Lambda a. t : Pi a. B
    )
    $

    For example, the following program

    ```scheme
    (define (id x) x)
    ```

    would have a type: `?a -> ?a`. We can postpone evaluation to get what's `?a` by unification algorithm. The output of `(id k)` depends on the type of `k`.

2. dependent type: $lambda Pi$(lambda Pi or lambda P)

    This system allows *type depends on term*, by the following rule.

    $
    frac(
      Gamma"," x : A tack.r B : *,
      Gamma tack.r Pi_(x : A) B_x : *
    )
    $

    $A : *$ says $A$ is a type, this rule says: when

    If a context with `x` is a `A` can shows `B` is a type, then $Pi_(x : A) B_x$ is a type in that context.

3. type operator: $lambda omega$(lambda omega)

   This system provides **type depends on type**, for example: `(List A)`, `(Tree A)`.

Combine them we can get

1. $F omega$
2. $lambda Pi 2$
3. $lambda Pi_omega$

and all of them $lambda C$, the *calculus of construction* system, in following chapters **dependent type** usually just refer to this system rather than $lambda Pi$.
