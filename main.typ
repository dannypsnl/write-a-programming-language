#import "template.typ": *

// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#show: project.with(
  title: "Write you a programming language",
  authors: (
    "林子篆",
  ),
)

#outline()

= Chapter 1: Introduction

Without type checking, program still works. Then why type checking? The process spent times, right? Yes, type checking uses more resource if we always make correct software. Unfortunately, we don't. For example, we might exceptionally write:

```js
"hello, " ++ 1
```

Who is `1`? That's a bit ridiculous unless we start naming people in number. How about `hello name = "hello, " ++ name`? This program can happen, or we write it again, but with check (in untyped scheme):

```racket
(define (hello name)
  (when (string? name)
    (string-append "hello, " name)))
```
