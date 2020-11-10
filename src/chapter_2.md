# Type Inference

With type, we need to define all our variable like the following code:

```racket
(define i : Number 1)
(define b : Bool #t)
(define c : Char #\c)
```

Annoying, we know what is `1`, `#t`, and `#\c`. Since we have knowledge, we can create a compiler find types for you:

```racket
(define (infer-type exp)
  (cond
    [(number? exp) 'Number]
    [(boolean? exp) 'Bool]
    [(char? exp) 'Char]))
```

Then we only have to write:

```racket
(define i 1)
(define b #t)
(define c #\c)
```
