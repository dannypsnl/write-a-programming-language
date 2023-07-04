= Type System

Without type checking, program still works. Then why type checking? The process spent times, right? Yes, type checking uses more resource if we always make correct software. Unfortunately, we don't. For example, we might exceptionally write:

```js
"hello, " ++ 1
```

Who is `1`? That's a bit ridiculous unless we start naming people in number. How about `hello name = "hello, " ++ name`? This program can happen, or we write it again, but with check (in untyped scheme):

```rkt
(define (hello name)
  (when (string? name)
    (string-append "hello, " name)))
```

This time we always have a string, ignore others, but maybe wee would like to know this function failed?

```rkt
(define (hello name)
  (if (string? name)
    (string-append "hello, " name)
    (error 'hello "name must be a string")))
```

Or we check it when compiling?

```hs
hello :: String -> String
hello name = "hello, " ++ name
```

If we only care about the runtime speed, this is the correct answer. With more type(information), better output from compiler was possible. Unfortunately, from another perspective, compile time can be hurt. Runtime performance is not the only consideration in the real world, what does it mean? In language like Coq, Agda, proof a list has expected length in compile time is possible, but they might take a crazy long time to compile a small program. Simpler languages like Rust, Haskell, still known as having slow compiler. More problems to solve, more time need. Designing a language, means make a balance between these choose, include but not just compile time problem.

#include "poly.typ"
#include "lc.typ"
#include "inference.typ"
#include "infer-poly.typ"
#include "lc-cube.typ"
#include "dt.typ"
#include "curry-howard.typ"
