package core.deltas.path

import core.language.SourceElement
import core.language.node._

object NodePath {
  implicit def toSimpleObject(withOrigin: NodePath): Node = withOrigin.current
  implicit def castList(list: Seq[NodePath]): Seq[Node] = list.map(x => x.current)
}

trait AnyPath extends SourceElement {

  def uriOption: Option[String]
}

trait NodePath extends NodeLike with AnyPath {
  type Self = NodePath
  def current: Node
  def parentOption: Option[NodePath]

  def findAncestorShape(shape: NodeShape): NodePath = ancestors.find(p => p.shape == shape).get
  def ancestors: Stream[NodePath] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def pathAsString: String

  override def toString = s"Path: $pathAsString\nCurrent: $current\nRoot: ${root.current}"
  def root: NodePath = ancestors.last

  override def asNode: Node = current
  override def asPath: Option[NodePath] = Some(this)

  def shape: NodeShape = current.shape
  def shape_=(value: NodeShape): Unit = current.shape = value

  def apply(key: NodeField): Any = get(key).get
  def update(key: NodeField, value: Any): Unit = current(key) = value
  def get(key: NodeField): Option[Any] = current.data.get(key).map {
    case _: Node => new NodeFieldValue(this, key)
    case sequence: Seq[_] => sequence.indices.map(index => {
      val element = sequence(index)
      element match {
        case _: Node => SequenceElement(this, key, index)
        case value => value
      }
    })
    case _ => FieldValue(this, key)
  }

  def getValue(key: NodeField): Any = current.data(key)

  override def dataView: Map[NodeField, Any] = current.data.keys.map(key => (key,apply(key))).toMap

  def getMember(field: NodeField): SourceElement = ValuePath(this, field)

  /*
  A None value means the Path is above the file level.
   */
  override def uriOption: Option[String] = current.startOfUri

  //TODO replace this with some NodePath 'view' to improve performance.
  def stopAt(predicate: NodePath => Boolean): NodePath = {
    if (predicate(this))
      return PathRoot(current)

    this match {
      case nfc: NodeFieldValue => new NodeFieldValue(nfc.parent.stopAt(predicate), nfc.field)
      case SequenceElement(parent, field, index) => SequenceElement(parent.stopAt(predicate), field, index)
    }
  }
}
