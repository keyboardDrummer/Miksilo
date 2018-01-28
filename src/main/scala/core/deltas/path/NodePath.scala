package core.deltas.path

import core.deltas.node.{Node, NodeField, NodeLike, NodeShape}
import core.language.SourceElement

object NodePath {
  implicit def toSimpleObject(withOrigin: NodePath): Node = withOrigin.current
  implicit def castList(list: Seq[NodePath]): Seq[Node] = list.map(x => x.current)
}

trait Path extends SourceElement {
  val current: Any
  def parentOption: Option[NodePath]


  def findAncestorShape(shape: NodeShape): NodePath = ancestors.find(p => p.shape == shape).get
  def ancestors: Stream[NodePath] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def pathAsString: String

  override def toString = s"Path: $pathAsString\nCurrent: $current\nRoot: ${root.current}"
  def root: NodePath = ancestors.last
}

trait NodePath extends NodeLike with Path {
  type Self = NodePath
  override val current: Node

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