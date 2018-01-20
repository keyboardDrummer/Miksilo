---
title: Node
category: Core
order: 3
---

In Miksilo, the abstract-syntax tree (AST) is untyped. This is at the heart of what makes Miksilo modular. However, working without types in Scala, a typed language, produces code that seems far from idiomatic. This article describes the types involved in defining Miksilo AST's, and the idiomatic way to use them.

[Node](https://github.com/keyboardDrummer/Miksilo/blob/master/src/main/scala/core/deltas/node/Node.scala) is the type used to define AST nodes in Miksilo. A node only has two properties: `data: Map[NodeField, Any]` and `shape: NodeShape`. The value of `shape` indicates which keys `data` contains, and can be interpreted as a run-time type. Instances of `NodeShape` and `NodeField` are commonly defined using a Scala object, which defines a unique value.

Here is an example of how you could define the `NodeShape` and `NodeField` values for a binary addition type. You can see that there is a `create` method defined on `NodeShape`.

```scala
object AdditionDelta extends Delta {
  def createAddition(left: Node, right: Node): Node =
    Shape.create(Left -> first, Right -> second)

  object Shape extends NodeShape
  object Left extends NodeField
  object Right extends NodeField
}
```

A data structure that compliments `Node` is `Path`, which describes a path from one `Node`, usually the root node of the program, to another. For the readers familiar with the concept of a [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)): a `Path` is zipper for programs. `Path` is useful because it allows one to traverse not just forward but also backward in the tree. Also, two nodes in different locations in a program may be equal, such as the 2's in `2 + 2`. However, two paths in a program are never equal, so a path can be useful to identify specific a specific part of a program.

Miksilo relies on the Nodes in the AST to be untyped, but sometimes we know the shape of a node, and we'd like to make accessing that node easier and safer by using that knowledge. For this case, we can define a typed wrapper around a `NodeShape`. Here is a typed wrapper for `AdditionDelta.Shape` from earlier:

```scala
implicit class Addition[T <: NodeLike](val node: T) extends NodeWrapper[T] {
  def left: T = node(Left).asInstanceOf[T]
  def left_=(value: T): Unit = node(Left) = value

  def right: T = node(Right).asInstanceOf[T]
  def right_=(value: T): Unit = node(Right) = value
}
```

The generic type `T` and the type `NodeLike` are used to make the wrapper work on both `Node` and `Path` types. Because `Addition` is an implicit class, we can access `left` and `right` on any value of type `Node`, and Scala will automatically know to use the getters and setters from `Addition`.

If you feel that most of what you've read in this article smells of boilerplate, then don't fret. Instead, read the follow-up article, [no boilerplate](http://keyboarddrummer.github.io/Miksilo/core/no-boilerplate/), to find a solution.