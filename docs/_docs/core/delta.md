---
title: Delta
category: Core
order: 2
---

In Blender, language transformations are packaged as re-usable units. Such a unit is called a Delta. Commonly, a delta applies a small change to a language, such as adding or removing a language feature, or adding an optimization. Languages are defined by composing many Delta's. By keeping Delta's simple and language agnostic, they can be reused for different languages. A similar approach is described in the paper [A Nanopass Framework for Compiler Education](https://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf).

Delta's can, and commonly do, depend on other Delta's. For example, [WhileContinueDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileContinueDelta.scala), that allows using the continue statement in a Java while loop, depends on [WhileDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileDelta.scala), that defines a Java while loop, which depends on [StatementSkeleton](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/StatementSkeleton.scala) that defines the concept of a statement, etc.

> Discuss compilation
> Discuss language class

An overview of Delta's and their dependencies can be seen in the _architecture panel_, which is part of the Blender desktop application.

> Add link
