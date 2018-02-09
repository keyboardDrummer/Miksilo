package core.deltas.path

import core.deltas.node.NodeField

case class FieldValue(parent: NodePath, field: NodeField) extends ChildPath {
  val current = parent.current(field)

  override def parentOption: Option[NodePath] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: FieldValue => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }

  override def replaceWith(replacement: Any): Unit = parent(field) = replacement //TODO hier hoort nog .obj. Hoezo compiled dit?

  override def pathAsString: String = s"${parent.pathAsString}/$field"
}


