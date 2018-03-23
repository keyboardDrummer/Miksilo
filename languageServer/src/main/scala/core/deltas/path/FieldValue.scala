package core.deltas.path

import core.language.node.{Node, NodeField, SourceRange}

case class FieldValue(parent: NodePath, field: NodeField) extends ChildPath {
  val current: Node = parent.current(field).asInstanceOf[Node]

  override def parentOption: Option[NodePath] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: FieldValue => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }

  override def replaceWith(replacement: Any): Unit = parent(field) = replacement //TODO hier hoort nog .obj. Hoezo compiled dit?

  override def pathAsString: String = s"${parent.pathAsString}/$field"

  override def position: Option[SourceRange] = parent.current.sources.get(field).orElse(current.position)
}


