package core.deltas.path

import core.deltas.node.{Node, NodeField, NodeLike, NodeShape}

object NodePath {
  implicit def toSimpleObject(withOrigin: NodePath): Node = withOrigin.current
  implicit def castList(list: Seq[NodePath]): Seq[Node] = list.map(x => x.current)
}

trait NodePath extends NodeLike with Path {
  type Self = NodePath
  override val current: Node


  override def asNode: Node = current
  override def asPath: Option[NodePath] = Some(this)
  override def getValue[T](key: NodeField): T = this(key).asInstanceOf[Path].current.asInstanceOf[T]
  override def setValue[T](key: NodeField, value: T): Unit = this(key).asInstanceOf[ChildPath].replaceWith(value)

  def shape: NodeShape = current.shape
  def shape_=(value: NodeShape): Unit = current.shape = value

  def apply(key: NodeField): Any = get(key).get
  def update(key: NodeField, value: Any): Unit = current(key) = value
  def get(key: NodeField): Option[Any] = current.data.get(key).map {
    case _: Node => new NodeFieldValue(this, key)
    case sequence: Seq[_] => sequence.indices.map(index => { //TODO misschien hier een SequencePath opleveren.
      val element = sequence(index)
      element match {
        case _: Node => new NodeSequenceElement(this, key, index)
        case _ => SequenceElement(this, key, index)
      }
    })
    case _ => FieldValue(this, key)
  }

  override def dataView: Map[NodeField, Any] = current.data.keys.map(key => (key,apply(key))).toMap
}