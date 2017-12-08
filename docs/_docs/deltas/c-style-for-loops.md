---
title: Interactions between independent Delta's
category: Deltas
order: 4
---

In this article we'll give an example of an interaction between two independent delta's, leading to a confusing language feature. We then introduce a third delta, which depends on the other two, to patch the language.

Our example starts with the delta [WhileLoop](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileLoopDelta.scala), which adds a while loop to the language. On top of the while loop, we can define to independent delta's:
- [WhileContinue](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileContinueDelta.scala) which introduces the continue statement for while loops.
- [ForLoop](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/ForLoopDelta.scala), which adds a C-style for loop to the language. It depends on the WhileLoop delta because it compiles by transforming the for-loop to an equivalent while-loop.

Here is the compilation code from [ForLoop](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/ForLoopDelta.scala):

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
The code is straightforward, it traverses over all for loop in the program, and replaces them with equivalent while loops.

And here is the compilation code from [WhileContinueDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileContinueDelta.scala)
```
def transformProgram(program: Node, compilation: Compilation): Unit = {
    val startLabels = new mutable.HashMap[Path, String]()
    PathRoot(program).visitClass(ContinueKey, path => transformContinue(path, startLabels, compilation))
  }

  def transformContinue(continuePath: Path, startLabels: mutable.Map[Path, String], language: Language): Unit = {
    val containingWhile = continuePath.findAncestorClass(WhileLoopDelta.WhileKey)
    val label = startLabels.getOrElseUpdate(containingWhile, addStartLabel(containingWhile))
    continuePath.replaceWith(JustJavaGoto.goto(label))
  }

  def addStartLabel(whilePath: Path): String = {
    val method = whilePath.findAncestorClass(MethodDelta.Clazz)
    val startLabel = LabelDelta.getUniqueLabel("whileStart", method)
    whilePath.asInstanceOf[SequenceElement].replaceWith(Seq(JustJavaLabel.label(startLabel), whilePath.current))
    startLabel
  }
```
For each while-loop containing a continue, as start label is placed before the while loop, and the continue's are translated to go-to statements that target the start label.

Now let's see what happens when we combine all three delta's. Actually this depends on the order in which we combine them. We have two choices:

1. `[WhileContinue, ForLoop, WhileLoop]`
1. `[ForLoop, WhileContinue, WhileLoop]`

`WhileLoop` must always come last since it's a dependency of the other two. If we choose option 1 then we basically avoid interaction between `WhileContinue` and `ForLoop`, since when `WhileContinue` no for-loops have been translated to while-loops yet.




