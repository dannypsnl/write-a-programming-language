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

In this section we would use Antlr to describe syntax and use Antlr as parser generator. Parser can be generated? Sure, but I
do not recommend it in production. But for simple stuff it was fine.

Here we were going to talk about natural number arithmetic syntax which supprts plus: `+`, times: `*`, minus: `-` and divide: `/`.

```antlr
expression
   : expression  (TIMES | DIV)  expression
   | expression  (PLUS | MINUS) expression
   | NUMBER
   ;
NUMBER : ('0' .. '9')+;
PLUS : '+';
MINUS : '-';
TIMES : '*';
DIV : '/';
WS : [ \r\n\t] + -> skip;
```

This is a very short syntax, even C lanugage syntax has 954 lines: https://github.com/antlr/grammars-v4/blob/master/c/C.g4 , cpp even has 1940 lines: https://github.com/antlr/grammars-v4/blob/master/cpp/CPP14.g4 .

Forget about that, at here code generator was really useful, we can quickly generate the Parser for our purpose:

```
TODO: antlr commands
```

TODO: Write an interpreter for arithmetic

TODO: Write a manual arithmetic parser and why we break them down to lexer for token stream.

## Lexer

Lexer is an optional, the correct way to describe it was a helper component, we would need it when the token was trying to reduce the concept we have to consider. If we don't use lexer, when we parsing

```
class Foo {}
```

we could write down:

```
identifier = take_char_until_one_of([' ', '\n'])
if identifier == "class"
   name_of_class = take_char_until_one_of([' ', '\n'])
   require('{')
   require('}')
else
   // parse different rule
```

Which was very low-level program, we have to handle each space and newline and remember when we don't need them. For many language we can extract out lexer/tokenizer to do these. The idea was we don't have to directly work with string, but with token, a token could contain `location`, `content`, `type` these information to help parser keep doing the parsing. A lexer can directly skip whitespace and newline, update location information and normalize the content of token(for example we can parse int or parse float before the token sent to parser).

TODO: simple lexer implmentation, with location info

## Manual parser
## Combinator
