package core.deltas.path

import core.language.node._

object NodePath {
  implicit def toSimpleObject(withOrigin: NodePath): Node = withOrigin.current
  implicit def castList(list: Seq[NodePath]): Seq[Node] = list.map(x => x.current)
}

trait NodePath extends NodeLike with AnyPath {
  type Self = NodePath
  def current: Node

  override def asNode: Node = current
  override def asPath: Option[NodePath] = Some(this)

  def shape: NodeShape = current.shape
  def shape_=(value: NodeShape): Unit = current.shape = value

  def apply(key: NodeField): Any = get(key).get
  def update(key: NodeField, value: Any): Unit = current(key) = value
  def get(key: NodeField): Option[Any] = current.data.get(key).map {
    case _: Node => new NodeFieldPath(this, key)
    case sequence: Seq[_] => sequence.indices.map(index => {
      val element = sequence(index)
      element match {
        case _: Node => NodeSequenceElement(this, key, index)
        case value => value
      }
    })
    case _ => FieldPath(this, key)
  }

  def getValue(key: NodeField): Any = current.data(key)

  override def dataView: Map[NodeField, Any] = current.data.keys.map(key => (key,apply(key))).toMap

  def getSourceElement(field: NodeField): FieldPath = FieldPath(this, field)

  /*
  A None value means the Path is above the file level.
   */
  override def uriOption: Option[String] = current.startOfUri

  //TODO replace this with some NodePath 'view' to improve performance.
  def stopAt(predicate: NodePath => Boolean): NodePath = {
    if (predicate(this))
      return PathRoot(current)

    this match {
      case nfc: NodeFieldPath => new NodeFieldPath(nfc.parent.stopAt(predicate), nfc.field)
      case NodeSequenceElement(parent, field, index) => NodeSequenceElement(parent.stopAt(predicate), field, index)
    }
  }
}
