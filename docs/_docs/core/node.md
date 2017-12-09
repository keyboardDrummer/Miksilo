---
title: Node
category: Core
order: 3
---

In Blender, the abstract-syntax tree (AST) is untyped. This is at the heart of what makes Blender modular. However, working without types in Scala, a typed language, produces code that seems far from idiomatic. This article describes the types involved in defining Blender AST's, and the idiomatic way to use them.

[Node](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/core/deltas/node/Node.scala) is the type used to define abstract syntax tree nodes in Blender. A node only has two properties:
 - `data: Map[NodeField, Any]`
 - `clazz: NodeClass`

The value of `clazz` represents a run-time type that says something about what keys `data` contains.

In Blender, instances of `NodeClass` and `NodeField` are commonly defined using Scala's `object` concept, which defines a singleton.

Here is an example of how you could define the `NodeClass` and `NodeField` values for a binary addition type. You can see that there is a `create` method defined on `NodeClass`.

```scala
object AdditionDelta extends Delta {
  def createAddition(left: Node, right: Node): Node =
    Clazz.create(Left -> first, Right -> second)

  object Clazz extends NodeClass
  object Left extends NodeField
  object Right extends NodeField
}
```

A data structure that compliments `Node` is `Path`, which described a path from one `Node`, usually the root node of the program, to another `Node`. For the readers familiar with the concept of a zipper: a `Path` is zipper for `Node`. `Path` is useful because it allows one to traverse not just forward but also backward in the tree. Also, two Nodes in different locations in a program may be equal, such as the 2's in `2 + 2`. However, two Paths in a program are never equal, so a Path can be useful to identify specific a specific part of a program.

Blender relies on the Nodes in the AST to be untyped, but sometimes we know the type of a Node, and we'd like to make accessing that Node easier and safer by using that knowledge. For this case, we can define a typed wrapper around a `NodeClass`. Here is a typed wrapper for `AdditionDelta.Clazz` from earlier:

```scala
implicit class Addition[T <: NodeLike](val node: T) extends NodeWrapper[T] {
  def left: T = node(Left).asInstanceOf[T]
  def left_=(value: T): Unit = node(Left) = value

  def right: T = node(Right).asInstanceOf[T]
  def right_=(value: T): Unit = node(Right) = value
}
```

The generic type `T` and the type `NodeLike` are used to make the wrapper work on both `Node` and `Path` types. Because `Addition` is an implicit class, we can access `left` and `right` on any value of type `Node`, and Scala will automatically know to use the getters and setters from `Addition`.

If you feel that most of what you've read in this article smells of boilerplate, then don't fret. Instead, read the follow-up article: [no boilerplate](http://keyboarddrummer.github.io/Blender/core/no-boilerplate/) to find a solution.