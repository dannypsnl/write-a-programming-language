# Parser

I have to say I have no idea why most compiler books spent time on Parser, of course this is really complex topic,
but practical language usually didn't need the complex techiology. But at here I still would list the common tools
would be there for developing a Parser.

Before that, we must know why we need the Parser, the Parser was a tool to translate the language in our mind into another
language to run on certain environment. The most common example was compiler compile the language to assembly, but why
assembly language? Because the environment in case was OS provide the tool called assembler which can translate assembly
to machine code which can run on the real machine(or more specific, run by CPU). Or like TypeScript, Elm and GHCJs compile
the origin language to another high-level language JavaScript.

So the point was we would like to run a program, and we would find out how to run it on our target platform. When you learn
these stuffs, I believe there are many resources keep mention AST. But what's AST? AST was abstract syntax tree, or to be
honestly, our real language. Sure, the syntax was not language, or at least just the outlooking part. The all important things
are our mind concept, and the language just a way to express our mind with some trade-off. What trade-off? For example a
language can write:

```
print hello, world
```

Of course was possible, however, would be hard to read in more complex place. So rather than make Compiler handles it, we
choose to let people(programmer) handles `string`. Now the code became:

```
print "hello, world"
```

It looks more clear. But the example can be more complex,
remember currently we are say `print` takes `"hello, world"` to do something, now we extend the example:

```
print "hello, " user_input
```

`user_input` was another function which get input from user, no matter how did it work, now we have trouble:
We should **print** `hello, <function>` or `hello, Danny`(if user type in `Danny`)?

In fact, compiler can not do the decision for you, whatever which behavior it picked would make functionality missing.
Sometimes we are really want to print out the function value. So we introduce parenthesis in case:

```
print("hello, ", user_input)
# or
print "hello, " (user_input)
```

The first one was picked by C family, and second one was using by ML family, both has cons and pros. We would keep mention
these issues.

## Simple parser and why we have next section
## Lexer
## BNF
## Manual parser
## Combinator
