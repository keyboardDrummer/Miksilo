package core.deltas.path

import core.language.node.{Node, NodeField, SourceRange}

class NodeFieldPath(parent: NodePath, field: NodeField) extends FieldPath(parent, field) with NodeChildPath {

}

/** A path to a value in an AST. This is similar to a NodePath, except that the value doesn't have to be of type Node.
  */
case class FieldPath(parent: NodePath, field: NodeField) extends ChildPath {
  def current: Node = parent.current(field).asInstanceOf[Node]

  override def parentOption: Option[NodePath] = Some(parent)

  override def pathAsString: String = s"${parent.pathAsString}/$field"

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: FieldPath => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }

  override def replaceWith(replacement: Any): Unit = parent(field) = replacement //TODO hier hoort nog .obj. Hoezo compiled dit?


  override def range: Option[SourceRange] = parent.current.sources.get(field).orElse(current.position)
}


