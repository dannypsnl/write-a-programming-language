= Chapter 3: Lambda Calculus

*lambda calculus* created by three rules

- variable $x$
- abstraction $lambda x.M$
- application $M N$

With two types: *Base type* and *Arrow type*, *lambda calculus* became *Simply typed lambda calculus(STLC)*, the most important property of *STLC* was strong normalization, which means it produces type for all terms.

*STLC* puts type rules in to system, where points out an abstraction $lambda x : A.M$ has type $A -> B$ if $M$ has type $B$, and application $M N$ get type $B$ if $M$ has type $A -> B$ and $N$ has type $A$.
