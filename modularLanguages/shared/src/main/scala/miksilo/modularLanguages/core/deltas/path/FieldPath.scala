package miksilo.modularLanguages.core.deltas.path

import miksilo.modularLanguages.core.node.{Node, NodeField}
import miksilo.editorParser.parsers.editorParsers.OffsetPointerRange

class NodeFieldPath(parent: NodePath, field: NodeField) extends FieldPath(parent, field) with NodeChildPath {
  override lazy val current: Node = super[FieldPath].current.asInstanceOf[Node]
  override def rangeOption: Option[OffsetPointerRange] = super[FieldPath].rangeOption.orElse(super[NodeChildPath].rangeOption)
}

case class FieldPath(parent: NodePath, field: NodeField) extends ChildPath {

  override def parentOption: Option[NodePath] = Some(parent)

  override def pathAsString: String = s"${parent.pathAsString}/$field"

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: FieldPath => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }

  override def replaceWith(replacement: Any): Unit = parent(field) = replacement //TODO hier hoort nog .obj. Hoezo compiled dit?

  override def rangeOption: Option[OffsetPointerRange] = parent.current.sources.get(field)

  override def current = parent.current(field)
}


