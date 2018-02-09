package core.deltas.path

import core.deltas.node.NodeField

object NodeFieldValue {
  def apply(parent: NodePath, field: NodeField) = new NodeFieldValue(parent, field)
  def unapply(value: NodeFieldValue): Option[(NodePath, NodeField) ]= Some(value.parent, value.field)
}

class NodeFieldValue(parent: NodePath, field: NodeField) extends FieldValue(parent, field) with NodePath {
  override val current: Node = parent.current(field).asInstanceOf[Node]
}