package core.particles.path

import core.particles.node.Node

case class SequenceElement(parent: Path, field: Any, index: Int) extends OriginWithParent
{
  val current: Node = parent.current(field).asInstanceOf[Seq[Node]](index)
  def sequence: Seq[Path] = parent(field).asInstanceOf[Seq[Path]]
  def next: Path = sequence(index + 1)
  def hasNext: Boolean = sequence.length > (index + 1)

  def replaceWith(replacements: Seq[Any]): Option[Any] = {
    val originalSequence = parent.current(field).asInstanceOf[Seq[Path]]
    val newSequence = originalSequence.take(index) ++ replacements ++ originalSequence.drop(index + 1)
    parent.current(field) = newSequence
  }

  override def parentOption: Option[Path] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode() * index

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: SequenceElement => other.parent.equals(parent) && other.field.equals(field) && other.index == index
    case _ => false
  }


  override def replaceWith(replacement: Any): Unit = replaceWith(Seq(replacement))

  override def pathAsString: String = s"${parent.pathAsString}.$field[$index]"
}
