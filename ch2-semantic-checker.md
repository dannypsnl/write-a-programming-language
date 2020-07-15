# Semantic Checker

Semantic checking can be a super complicate issue, but rather than push you into the crazy formal world, I would only show a simple overview about semantic checking. Semantic checking, would usually get confused with the concept about type checking, but not. Semantic checking includes type checking. For example, we can say expression `1/0` is invalid, whatever in runtime or compile time(CPU handles this usually by the way, also can see as a runtime checking). Semantic checking can do much more in runtime, but find out troubles in compile time as possible make a more robust system. A type, can be considered as properties of an object, with checking we can avoid unexpected input, e.g. `(lambda ([x : Integer]) x)` never takes `"Hello"` as the parameter, compiler can find out this. Now we get the first example of type check: same type checking. What's the same can be much more complicate than you might expect, but here I focus on the concept of same type instance. In the example, `Integer` is a **builtin** type, which means there can have non-builtin type, for example in **C** we can have `struct XXX` or `typedef`. With these, we can have our first language:

```racket
; BTW, add #:transparent for all structs
;;; in Racket, * usually stands for many
(struct Struct (name field*))
(struct Field (typ name))
(struct term)
(struct Struct/value term (struct-name term*))
(struct Int term (value))
(struct Var term (typ name))
(struct Func term (var term))
(struct Func/call term (term term))
```

We can simpling assuming there has no syntax error since parser should handle this than use valid abstraction syntax tree.

## Lambda Cube

### lambda calculus

### Simply typed lambda calculus

### polymorphism

#### unification

#### let polymorphism

### dependent type

#### types depends on terms

### calculus of construction
