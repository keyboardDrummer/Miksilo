package core.deltas.path

import core.language.node.{Node, NodeField, SourceRange}

case class SequenceElement(parent: NodePath, field: NodeField, index: Int) extends ChildPath
{
  val current: Node = parent.current(field).asInstanceOf[Seq[Node]](index)
  def sequence: Seq[NodePath] = parent(field).asInstanceOf[Seq[NodePath]]
  def next: NodePath = sequence(index + 1)
  def hasNext: Boolean = sequence.length > (index + 1)
  def getNext: Option[NodePath] = if (hasNext) Some(next) else None

  def replaceWith(replacements: Seq[Any]): Unit = {
    val originalSequence = parent.current(field).asInstanceOf[Seq[NodePath]]
    val newSequence = originalSequence.take(index) ++ replacements ++ originalSequence.drop(index + 1)
    parent.current(field) = newSequence
  }

  override def parentOption: Option[NodePath] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode() * index

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: SequenceElement => other.parent.equals(parent) && other.field.equals(field) && other.index == index
    case _ => false
  }

  override def replaceWith(replacement: Any): Unit = replaceWith(Seq(replacement))

  override def pathAsString: String = s"${parent.pathAsString}/$field[$index]"

  override def range: Option[SourceRange] = current.asNode.position
}
