# Parametric polymorphism

**Parametric polymorphism** also known as generic, it provides ability to abstract a type, in **STLC** we have abstraction which looks like \\( \lambda x : T .M \\). If `x` is a type? We usually use `*` for type of type. Now we get \\( \lambda x : \* .M \\), an abstraction of a type. For example, `List<T>` is a function `List : (T : *) -> *`.

From implementation perspective, we have two solutions:

1. dynamic
2. generate

**Dynamic** is the simple one for code generation, from C perspective it might just a `void *`, thus

```c
struct Node<T> {
    Node<T> *prev;
    T value;
}
```

would be:

```c
struct Node {
    Node *prev;
    void *value;
}
```

**Generate** is hard one, but can get better performance, still the same `Node<T>`, we generate `Node<i64>` to:

```c
struct Node_i64 {
    Node_i64 *prev;
    i64 value;
}
```

`Node<f64>` to:

```c
struct Node_f64 {
    Node_f64 *prev;
    f64 value;
}
```

However, problem of **Generate** method is obviously, it produces larger binary, and the performance issue of **Dynamic** method can be solved by JIT.
