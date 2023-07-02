#import "template.typ": *

// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#show: project.with(
  title: "Write you a programming language",
  authors: (
    "Lîm Tsú-thuàn",
  ),
)

#set heading(numbering: "1.")

#outline()

= Overview

Originally, I follow most traditional course and trying to tell how to make a programming language from technology view, but I quickly found how wrong this way. Because we already have a lot of language, another bad language is not need. Existed tutorials already enough to help anyone make a compiler, but how to design a good language is a question, you would usually need to read tons of books in PLT(Programming Language Theory) this area, and be carefully to avoid be killed by math. I won't try to say what I made is the best, but giving a lot of independent features, and let readers feel the core concept behind them to help readers make a good design.

#include "chapter/ch1.typ"
#include "chapter/ch2.typ"
#include "chapter/ch3.typ"
#include "chapter/ch4.typ"
#include "chapter/ch5.typ"
#include "chapter/ch6.typ"
#include "chapter/ch7.typ"
#include "chapter/ch8.typ"

= Appendix

#include "appendix/parser.typ"