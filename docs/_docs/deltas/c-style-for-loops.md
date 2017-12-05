---
title: C-style for loops
category: Deltas
order: 4
---

The delta [ForLoopDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/ForLoopDelta.scala), which adds a C-style for loop to the language, is implemented with little code because it is compiled by transforming it to an equivalent while loop.

The code for compiling the for loop is:

```scala
  override def transformProgram(program: Node, state: Compilation): Unit = {
    PathRoot(program).visitClass(ForLoopType).foreach(transformForLoop)
  }

  def transformForLoop(forLoopPath: Path): Unit = {
    val forLoop: ForLoop[Node] = forLoopPath.current
    val whileBody = forLoop.body ++
      Seq(ExpressionAsStatementDelta.create(forLoop.increment))
    val _while = WhileDelta.create(forLoop.condition, whileBody)

    val newStatements = Seq[Node](forLoop.initializer, _while)
    forLoopPath.asInstanceOf[SequenceElement].replaceWith(newStatements)
  }
```

The code shown here is rather straightforward, it traverses over all for loop in the program, and replaces them with equivalent while loops.

