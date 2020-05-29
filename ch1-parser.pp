#lang pollen

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

In this section we would use Perl6 to build a parser. Parser can be generated? Sure, but I do not recommend it in production. But for simple stuff it was fine.

Here we were going to talk about natural number arithmetic syntax which supprts plus: `+`, times(multiple): `*`, minus: `-` and divide: `/`.

```perl6
grammar Calculator {
    token TOP { <calc-op> }

    proto rule calc-op          {*}
          rule calc-op:sym<mult> { <num> '*' <num> }
          rule calc-op:sym<div> { <num> '/' <num> }
          rule calc-op:sym<add> { <num> '+' <num> }
          rule calc-op:sym<sub> { <num> '-' <num> }
    # just like regex, \d+ is at least one digit
    token num { \d+ }
}
```

This is a very short syntax, even C lanugage syntax has 954 lines: https://github.com/antlr/grammars-v4/blob/master/c/C.g4 , cpp even has 1940 lines: https://github.com/antlr/grammars-v4/blob/master/cpp/CPP14.g4 in Antlr4(another parser generator).

Forget about that, at here code generator was really useful, we can quickly generate the Parser for our purpose. Then we can create an interpreter based on it:

```perl6
class Calculations {
    method TOP              ($/) { make $<calc-op>.made; }
    # if you are not familiar with Perl just like me, notice that `calc-op` mapping to each grammar
    method calc-op:sym<mult> ($/) { make [*] $<num> }
    method calc-op:sym<div> ($/) { make [/] $<num> }
    method calc-op:sym<add> ($/) { make [+] $<num>; }
    method calc-op:sym<sub> ($/) { make [-] $<num>; }
}

say '2+2 = ' ~ Calculator.parse('2+2', actions => Calculations).made;
say '2*3 = ' ~ Calculator.parse('2*3', actions => Calculations).made;
```

This one basically not good enough, it can't handle parentheses, can't handle `2 * 2 + 3`. But anyway shows how interpreter work.

Now consider a manual parser, how would it looks like? It actually easy to build up. Consider the following Java program:

```java
Scanner s = new Scanner(input);
StringBuilder number = new StringBuilder("");
char c = s.next().charAt(0);
while (Character.isDigit(c)) {
    number.append(c);
    c = s.next().charAt(0);
}
// now c must not a digit
while (Character.isSpace(c)) {
    c = s.next().charAt(0); // skip whitespace
}
if (c == '+') {
    c = s.next().charAt(0);
} else {
    throw new SyntaxException("allow + operator only");
}
while (Character.isSpace(c)) {
    c = s.next().charAt(0); // skip whitespace
}
StringBuilder number2 = new StringBuilder("");
char c = s.next().charAt(0);
while (Character.isDigit(c)) {
    number2.append(c);
    c = s.next().charAt(0);
}
return new BinaryExpression(number, number2, Operator("+"));
```

This is of course too exaggerated, but can show why handling input stream is not a good idea. That's why we introduce Lexer layer.

## Lexer

Lexer is an optional, the correct way to describe it was a helper component, we would need it when the token was trying to reduce the concept we have to consider. If we don't use lexer, when we parsing

```
class Foo {}
```

we could write down:

```py
identifier = take_char_until_one_of([' ', '\n'])
if identifier == "class":
    name_of_class = take_char_until_one_of([' ', '\n'])
    require('{')
    require('}')
else:
    // parse different rule
```

Which was very low-level program, we have to handle each space and newline and remember when we don't need them. For many language we can extract out lexer/tokenizer to do these. The idea was we don't have to directly work with string, but with token, a token could contain `location`, `content`, `type` these information to help parser keep doing the parsing. A lexer can directly skip whitespace and newline, update location information and normalize the content of token(for example we can parse int or parse float before the token sent to parser).

TODO: simple lexer implmentation, with location info

## Manual parser

A manual parser is powerful, but on the other hand it takes a lot of effort. Before you jump into writing a manual parser and never go back again, ensure that parser generator cannot handle your case.

Write a manual parser didn't need many parsing background knowledge, surprising, but heavy repetitive work. Because simply porting **LL** syntax can handle about 90% job. For example, an assignment syntax `<type> <name> = <expr>;` can use the following parser:

```py
typ = parse_type()
name = parse_identifier()
expect_symbol('=')
expr = parse_expr()
expect_symbol(';')
return Assign(typ=typ, name=name, expr=expr)
```

However, there has an annoying case: **expression parsing**. How this became the big problem for newbies? If we follow **LL** strict conversion from syntax as below:

```bnf
expr ::=
  expr "*" expr
  | expr "+" expr
  | // ignore others
```

the conversion is:

```py
def parse_expr():
    left_expr = parse_expr()
    op = expect_oneof('*', '+')
    right_expr = parse_expr()
    return BinaryExpression(left_expr, op, right_expr)
```

This is **left recursive**, your parser would keep calling `parse_expr` and never end(or stack overflow, depend on which language you're using).

Clever as you, might thought out how to fix this quickly:

```bnf
expr ::=
  integer "*" expr
  | integer "+" expr
  | integer
```

here is conversion:

```py
def parse_expr():
    left_expr = parse_integer()
    try:
        op = expect_oneof('*', '+')
        try:
            right_expr = parse_expr()
            return BinaryExpression(left_expr, op, right_expr)
        except:
            # keep throw up the parse error
            raise
    except:
        # to simplify example, assuming `expect_oneof` is under the control and won't throw unexpected exception
        # no right hand side expression
        return left_expr
```

It would work, until you have to handle **operator precedence**. For example, `*` would usually be applied before `+`. As clever as you, get the solution quickly again:

```bnf
expr ::=
  term "*" expr
  | term
term ::=
  integer "+" expr
  | integer
```

However, the implementation work became quick heavy now.

To solve all problem in once, yeah, we need a better way to handle this.

TODO: Pratt Parsing

## Combinator
