---
title: No boilerplate
category: Core
order: 4
---

> This article is under construction.

To use Blender effectively, we have to write some boilerplate. For each type of `Node`, we need to define one `NodeClass` object, and a `NodeField` object for each field. Then, to make using the accessing fields of the new Node type easier, we need a subclass of `NodeWrapper`. In the future, Scala macro's might help us to generate this boilerplate, but currently they are not mature enough. For now we've written a generator instead. Here follows an example of the generator in action:

Given the following input:

```scala
object ClassFileDelta {
  val input = new NodeClassDefinition("ClassFile", 
    "classInfoIndex" -> "Int",
    "interfaces" -> "Seq[Int]",
    "methods" -> wrapSeq("Seq[MethodInfo]"),
    "attributes" -> "Seq[Node]"
  )
}
```
It outputs:

```scala
object ClassFileDelta {
  val input = new NodeClassDefinition("ClassFile", 
    "classInfoIndex" -> "Int",
    "interfaces" -> "Seq[Int]",
    "methods" -> wrapSeq("Seq[MethodInfo]"),
    "attributes" -> "Seq[Node]"
  )

  //region Generated Node boilerplate
  object Clazz extends NodeClass
  object Interfaces extends NodeField
  object Methods extends NodeField
  object Attributes extends NodeField

  implicit class ClassFile[T <: NodeLike](val node: T) extends NodeWrapper {
    assert(node.clazz == Clazz)

    def interfaces: Seq[Int] = node(Interfaces).asInstanceOf[Seq[Int]]
    def interfaces_(value: Seq[Int]): Unit = node(Interfaces) = value

    def methods: Seq[MethodInfo[T]] = NodeWrapper.wrapSeq(node(Methods).asInstanceOf[Seq[T]])
    def methods_(value: Seq[MethodInfo[T]]): Unit = node(Methods) = NodeWrapper.unwrapSeq(value)

    def attributes: Seq[T] = node(Attributes).asInstanceOf[Seq[T]]
    def attributes_(value: Seq[T]): Unit = node(Attributes) = value
  }  
  //endregion
}
```

The generic type argument for the wrappers allows it to wrap around both `Node` and `Path` types. Wrapped fields can either wrap around regular types, other wrappers, or sequences of other wrappers.