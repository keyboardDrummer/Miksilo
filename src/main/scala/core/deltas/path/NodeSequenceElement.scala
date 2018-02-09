package core.deltas.path

import core.deltas.node.{Node, NodeField}

object NodeSequenceElement {
  def unapply(value: NodeSequenceElement): Option[(NodePath, NodeField, Int)] = Some(value.parent, value.field, value.index)
}

class NodeSequenceElement(parent: NodePath, field: NodeField, index: Int) extends SequenceElement(parent, field, index)
  with NodePath
{
  override val current: Node = parent.current(field).asInstanceOf[Seq[Node]](index)
  override def sequence: Seq[NodePath] = parent(field).asInstanceOf[Seq[NodePath]]
  override def next: NodePath = sequence(index + 1)
}
