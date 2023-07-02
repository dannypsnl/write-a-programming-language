= Chapter 4: Polymorphism & Advanced Type Inference

Polymorphism is all about how to handle more generally input via some constraints. Here we are going to talk about

#include "ch4-ad-hoc-poly.typ"
#include "ch4-parametric-poly.typ"

== Subtyping

*Subtyping* is a predicate between two types, describe a type can be treat as another type. Its formal syntax usually looked like this: `A <: B`(`A` is subtype of `B`), which means, a binding `B` can have value has type `A`. *Subtyping* was wildly used in many class-based languages, but it also brings new problem when working with **Ad-hoc**. For example: `f : B -> B` usually can apply with `f (a : A)` when has *subtyping*, however, if we also have overloading function `f : A -> B`? We have three choices here:

1. use `A -> B`
2. use `B -> B`
3. report conflict error

All of them are fine, it depends on which one you tend to have, this is just trade off.

== Row Polymorphism

Row polymorphism helps us get **a structure only when it with expected properties**. For example, we can have a function type: `{a : A, rest : ...} -> {a : A}`, where `{a = A()}`, `{a = A(), b = B(), c = C()}`, and `{a = A(), b = B()}` are all valid argument, but `{a = B()}`, `{b = B()}`, and `{b = B(), c = C()}` are not.
