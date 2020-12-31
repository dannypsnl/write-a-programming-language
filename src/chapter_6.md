# row polymorphism

Row polymorphism helps us get **a structure only when it with expected properties**. For example, we can have a function type: `{a : A, rest : ...} -> {a : A}`, where `{a = A()}`, `{a = A(), b = B(), c = C()}`, and `{a = A(), b = B()}` are all valid argument, but `{a = B()}`, `{b = B()}`, and `{b = B(), c = C()}` are not.
