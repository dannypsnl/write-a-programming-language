# Type Checking

Without type checking, program still works. Oh! Then why type checking? They spent times, right? Yes, type checking use more resource if we **always** make correct software. Unfortunately, we don't. For example, we might exceptionally write:

```racket
(string-append "hello, " 1)
```

That's ridiculous, but if `(define (hello name) (string-append "hello, " name))`? The previous program can happen, when we didn't write:

```racket
(define (hello name)
  (when (string? name)
    (string-append "hello, " name)))
```

This time we always have a string, ignores others, but maybe we would like to know this function failed?

```racket
(define (hello name)
  (if (string? name)
    (string-append "hello, " name)
    (error 'hello "name should be a string!")))
```

Or, we can just check it when compiling?

```racket
(define (hello [name : String])
  (string-append "hello, " name))
```
