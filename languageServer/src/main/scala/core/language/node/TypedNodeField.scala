package core.language.node

class TypedNodeField[T](name: String) extends NodeField {
  def get(node: Node): Option[T] = node.get(this).asInstanceOf[Option[T]]
  def apply(node: Node): T = node(this).asInstanceOf[T]
  def update(node: Node, value: T): Unit = node(this) = value

  override lazy val toString = name
}
