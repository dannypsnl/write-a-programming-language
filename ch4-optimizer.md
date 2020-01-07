# Optimizer

After we generate the code, we would find there are many useless code in the langauge, then we need optimizer to generate better code.

In this section we would focus on optimizing assembly like language.

```racket
#lang typed/racket
(struct Block (
    [instructions : Listof Instruction]
    [prevs : Listof Block]
    [succs : Listof Block]
))

(struct DFG (
    [codeList : Listof Instruction]
    [blocks : Listof Block]
))
```

TODO: constant propagation
