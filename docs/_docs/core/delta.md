---
title: Delta
category: Core
order: 2
---

In Blender, language transformations are packaged as re-usable units. Such a unit is called a delta. Commonly, a delta applies a small change to a language, such as adding or removing a language feature, or adding an optimization. Languages are defined by composing many delta's. A similar approach is described in the paper [A Nanopass Framework for Compiler Education](https://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf).

Delta's can, and commonly do, depend on other delta's. For example, [WhileContinueDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileContinueDelta.scala), that adds a continue statement usable in a while loop, depends on [WhileDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileDelta.scala), that adds a while loop, which depends on [IfThenDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/IfThenDelta.scala), that adds an if statement, etc.

> Discuss compilation type

> Discuss language type

An overview of delta's and their dependencies can be seen in the _architecture panel_, which is part of the Blender desktop application.

> Add link
