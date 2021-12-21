# Chapter 1: Type Checking

Without type checking, program still works. Oh! Then why type checking? They spent times, right? Yes, type checking use more resource if we **always** make correct software. Unfortunately, we don't. For example, we might exceptionally write:

```scheme
(string-append "hello, " 1)
```

That's ridiculous, but if `(define (hello name) (string-append "hello, " name))`? The previous program can happen, when we didn't write:

```scheme
(define (hello name)
  (when (string? name)
    (string-append "hello, " name)))
```

This time we always have a string, ignores others, but maybe we would like to know this function failed?

```scheme
(define (hello name)
  (if (string? name)
    (string-append "hello, " name)
    (error 'hello "name should be a string!")))
```

Or, we can just check it when compiling?

```scheme
(define (hello [name : String])
  (string-append "hello, " name))
```

This is correct answer, if we only care about the runtime speed, more type(information), better output was possible. Unfortunately, from another perspective, compile time can be hurt. Runtime performance is not the only consideration, what does this mean? In language like **Coq**, **Agda**, proof a list has expected length in compile time is possible, but they might take a crazy long time to compile a small program. Simpler languages like **Rust**, **Haskell**, still known as having slow compiler, solve more problems in compile time make compile time longer. Designing language, means make a balance between these choose(include but not just compile time problem).
