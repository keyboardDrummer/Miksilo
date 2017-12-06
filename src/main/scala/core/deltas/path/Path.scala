package core.deltas.path

import core.deltas.node.{Node, NodeClass, NodeField, NodeLike}

object Path {
  implicit def toSimpleObject(withOrigin: Path): Node = withOrigin.current
  implicit def castList(list: Seq[Path]): Seq[Node] = list.map(x => x.current)
}

trait Path extends NodeLike { //TODO rename path to something that imports more easily.
  type Self = Path
  val current: Node

  override def toString = s"Path: $pathAsString\nCurrent: $current\nRoot: ${root.current}"
  def pathAsString: String
  def parentOption: Option[Path]
  def ancestors: Stream[Path] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def findAncestorClass(clazz: NodeClass): Path = ancestors.find(p => p.clazz == clazz).get
  def root = ancestors.last
  def clazz = current.clazz
  def clazz_=(value: NodeClass): Unit = current.clazz = value

  def apply(key: NodeField) = get(key).get
  def update(key: NodeField, value: Any): Unit = current(key) = value
  def get(key: NodeField): Option[Any] = current.data.get(key).map {
    case childObject: Node => FieldValue(this, key)
    case sequence: Seq[_] => sequence.indices.map(index => {
      val element = sequence(index)
      element match {
        case childObject: Node => SequenceElement(this, key, index)
        case _ => element
      }
    })
    case child => child
  }

  override def dataView: Map[NodeField, Any] = current.data.keys.map(key => (key,apply(key))).toMap
}