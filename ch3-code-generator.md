# Code Generator

There have many different kinds of IR(Intermediate Representation) in the world, but for convenience, I would only shows how to compile to a ASM like language. In such language, we will not have high-level conditional structure like **if**, **switch**, or repeated structure like **for**, **while**, one of our major tasks is using **goto** to build these high-level structures.

Generate **if** code is quite intuitive.
1. **bool** use `0` for `false`, non `0` for `true`
2. mark each block with a label
TODO
