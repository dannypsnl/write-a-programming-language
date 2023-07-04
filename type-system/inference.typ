== Type Inference

With type, we need to define all our variable like the following code:

```haskell
i :: Integer
i = 1

b :: Boolean
b = True

c :: Char
c = 'c'
```

This is annoying since we can see the type from primitive terms! Which means it's ok to rewrite above as the following, if your language supports type inference!

```haskell
i = 1
b = True
c = 'c'
```

A simple inference function for your language might like
```haskell
infer :: Term -> Type
infer (TmInt _) = TyInt
infer (TmBool _) = TyBool
infer (TmChar _) = TyChar
```
