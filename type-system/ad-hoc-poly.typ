=== Ad-hoc polymorphism

*Ad-hoc* can be simply understandsing as overloading (though that's to simple but enough for now), in Haskell you have `class` (not `class` in C++/Java), in Java you have overload function, there has no different down to the core. This feature allows a function definition works with different type. Two concrete approaches(Haskell and Java style) looks different at first:

- Haskell version

  ```haskell
  class Countable a where
    count :: a -> Int

  instance Countable [a] where
    count lst = length lst

  instance Countable Human where
    count Human { age=age } = age
  ```

- Java version

  ```java
  int count(List<T> lst) { return lst.size(); }
  int count(Human h) { return h.getAge(); }
  ```

Both implemented by the same way:

1. using encoding duplicate functions' name(ignore parametric polymorphism for now) and record these **instances**, for example:
   - Haskell get: `Countable.[a].count :: [a] -> Int`, `Countable.Human.count :: Human -> Int`
   - Java get: `count_List<T>`, `count_Human`
2. search *instances* for function call, for example:
   - `count [1, 2, 3]` get `Countable.[a].count :: [a] -> Int`
   - `count 1` get nothing and hence get an error

How about dynamic typing? Dynamic typed language still implements ad-hoc since they simply accept all types, which means the previous two-steps method didn't need by them, but the cost is them would crash in runtime if no *instance*.
