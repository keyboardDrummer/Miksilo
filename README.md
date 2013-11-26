Modular Compiler
===============

This is a pet project of mine. Its goal is to create a codebase for easily building compilers for different languages. Compilers are commonly composed of several phases, such as parsing and code generation. Often one or several intermediate languages are used to perform the code generation in steps, in order to reduce the compiler builder's heachache. 

This project tries to define a large number of small language transformations. Each transformation creates a new intermediate language. Transformations both consume and produce a contract, which defines which transformation can be combined and in what order.

The abstract syntax trees of the intermediate languages cannot be defined using Scala's type system. For example, transformations must be able remove a member from an AST node, which is not possible using OO inheritance. Because of this, the transformations operate on an untyped AST. Sadly this means we cannot leverage the Scala type checking for much of our code.
