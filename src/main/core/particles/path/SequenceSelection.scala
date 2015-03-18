package core.particles.path

import core.particles.MetaObject

case class SequenceSelection(parent: Path, field: Any, index: Int) extends OriginWithParent
{
  val current = parent.current(field).asInstanceOf[Seq[MetaObject]](index)
  def sequence: Seq[Path] = parent(field).asInstanceOf[Seq[Path]]
  def next = sequence(index + 1)
  def hasNext = sequence.length > (index + 1)

  def replaceWith(replacements: Seq[MetaObject]) = {
    val originalSequence = parent.current(field).asInstanceOf[Seq[Path]]
    val newSequence = originalSequence.take(index) ++ replacements ++ originalSequence.drop(index + 1)
    parent.current(field) = newSequence
  }

  override def parentOption: Option[Path] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode() * index

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: SequenceSelection => other.parent.equals(parent) && other.field.equals(field) && other.index == index
    case _ => false
  }


  override def replaceWith(replacement: MetaObject): Unit = replaceWith(Seq(replacement))
}
