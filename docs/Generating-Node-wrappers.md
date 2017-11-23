Sadly to use Blender effectively we need to write some boilerplate. For each new type of `Node`, we need to define one `NodeClass` object, and a `NodeField` object for each field. Then, to make using the new Node type easier, we need a `NodeWrapper` to make accessing the fields easier. Scala macro's might help us here but they are not mature enough. To help with writing boilerplate we've written a generator for this.

It takes the following as input:
```scala
object ClassFileDelta {
  val input = new NodeClassDefinition("ClassFile", 
    "classInfoIndex" -> "Int",
    "interfaces" -> "Seq[Int]",
    "methods" -> wrapSeq("Seq[MethodInfo]"),
    "attributes" -> "Seq[Node]"
  )
```
And then outputs:
```scala
object ClassFileDelta {
  val input = new NodeClassDefinition("ClassFile", 
    "classInfoIndex" -> "Int",
    "interfaces" -> "Seq[Int]",
    "methods" -> wrapSeq("Seq[MethodInfo]"),
    "attributes" -> "Seq[Node]"
  )

  //region Generated Node class boilerplate
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