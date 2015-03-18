package core.particles.path

import core.particles.node.MetaObject

case class Selection(parent: Path, field: Any) extends OriginWithParent {
  val current = parent.current(field).asInstanceOf[MetaObject]
  override def parentOption: Option[Path] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Selection => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }

  override def replaceWith(replacement: MetaObject): Unit = parent(field) = replacement //TODO hier hoort nog .obj. Hoezo compiled dit?
}
