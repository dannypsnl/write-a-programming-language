# Code Generator

Tree representation of a small register based machine.

```rkt
(op dst a b) ;;; op can be: add, sub, mul, div
(load dst src)
(store dst src)
(reg name)
(br to)
(br cond to) ;;; 0 is false, else true
```

Tree representation of a small IR.

```rkt
;;; statement
(= var exp)
(if cond goto-a-label)
(label name)
;;; exp
(+ op op)
(- op op)
(* op op)
(/ op op)
;;; op
integer
var
```

## X86

## LLVM
