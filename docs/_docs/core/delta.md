---
title: Delta
category: Core
order: 2
---

In Miksilo, language transformations are packaged as re-usable units. Such a unit is called a delta. Commonly, a delta applies a small change to a language, such as adding or removing a language feature, or adding an optimization. Languages are defined by composing many deltas. A similar approach is described in the paper [A Nanopass Framework for Compiler Education](https://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf).

Delta's can, and commonly do, depend on other deltas. For example, [WhileContinueDelta](https://github.com/keyboardDrummer/Miksilo/blob/master/src/main/scala/deltas/javac/statements/WhileContinueDelta.scala), that adds a continue statement usable in a while loop, depends on [WhileDelta](https://github.com/keyboardDrummer/Miksilo/blob/master/src/main/scala/deltas/javac/statements/WhileDelta.scala), that adds a while loop, which depends on [IfThenDelta](https://github.com/keyboardDrummer/Miksilo/blob/master/src/main/scala/deltas/javac/statements/IfThenDelta.scala), that adds an if statement, etc.

Related to the type `Delta` is `Language`, which is created from a list of deltas, and provides an API of language features such as parsing and compiling.

An overview of deltas and their dependencies can be seen when running the [Miksilo UI](http://keyboarddrummer.github.io/Miksilo/core/sandbox/) and navigation to the dependency graph.
