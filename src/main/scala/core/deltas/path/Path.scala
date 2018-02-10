package core.deltas.path

import core.deltas.node._
import core.language.SourceElement

object Path {
  implicit def toSimpleObject(withOrigin: Path): Node = withOrigin.current
  implicit def castList(list: Seq[Path]): Seq[Node] = list.map(x => x.current)
}

trait Path extends NodeLike with SourceElement {
  type Self = Path
  val current: Node
  def parentOption: Option[Path]

  def findAncestorShape(shape: NodeShape): Path = ancestors.find(p => p.shape == shape).get
  def ancestors: Stream[Path] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def pathAsString: String

  override def toString = s"Path: $pathAsString\nCurrent: $current\nRoot: ${root.current}"
  def root: Path = ancestors.last

  override def asNode: Node = current
  override def asPath: Option[Path] = Some(this)

  def shape: NodeShape = current.shape
  def shape_=(value: NodeShape): Unit = current.shape = value

  def apply(key: NodeField): Any = get(key).get
  def update(key: NodeField, value: Any): Unit = current(key) = value
  def get(key: NodeField): Option[Any] = current.data.get(key).map {
    case _: Node => FieldValue(this, key)
    case sequence: Seq[_] => sequence.indices.map(index => {
      val element = sequence(index)
      element match {
        case _: Node => SequenceElement(this, key, index)
        case value => value
      }
    })
    case value => value
  }

  override def dataView: Map[NodeField, Any] = current.data.keys.map(key => (key,apply(key))).toMap
}
