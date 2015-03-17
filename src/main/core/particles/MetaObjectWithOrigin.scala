package core.particles

trait Origin {
  def parentOption: Option[MetaObjectWithOrigin]
  def ancestors: Stream[MetaObjectWithOrigin] = parentOption.map(parent => parent #:: parent.origin.ancestors).getOrElse(Stream.empty)
}

trait OriginWithParent extends Origin {
  def parent: MetaObjectWithOrigin
}

object Root extends Origin with Key{
  override def parentOption: Option[MetaObjectWithOrigin] = None
}

case class Selection(parent: MetaObjectWithOrigin, field: Any) extends OriginWithParent {
  override def parentOption: Option[MetaObjectWithOrigin] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Selection => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }
}

case class SequenceSelection(parent: MetaObjectWithOrigin, field: Any, index: Int) extends OriginWithParent
{
  def sequence: Seq[MetaObjectWithOrigin] = parent(field).asInstanceOf[Seq[MetaObjectWithOrigin]]
  def next = sequence(index + 1)
  def hasNext = sequence.length > (index + 1)
  def current = sequence(index)
  def replaceWith(itemToInsert: Seq[MetaObject]) = {
    val originalSequence = parent.data(field).asInstanceOf[Seq[MetaObjectWithOrigin]]
    val newSequence = originalSequence.take(index) ++ itemToInsert ++ originalSequence.drop(index + 1)
    parent.obj.data(field) = newSequence
  }

  override def parentOption: Option[MetaObjectWithOrigin] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode() * index

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: SequenceSelection => other.parent.equals(parent) && other.field.equals(field) && other.index == index
    case _ => false
  }

  override def toString() = super.toString()
}

object MetaObjectWithOrigin {
  implicit def toSimpleObject(withOrigin: MetaObjectWithOrigin): MetaObject = withOrigin.obj
}

case class MetaObjectWithOrigin(obj: MetaObject, origin: Origin = Root) extends MetaLike
{
  def clazz = obj.clazz
  def apply(key: Any) = get(key).get
  def get(key: Any): Option[Any] = obj.data.get(key).map {
    case childObject: MetaObject => new MetaObjectWithOrigin(childObject, Selection(this, key))
    case sequence: Seq[_] => sequence.indices.map(index => {
      val element = sequence(index)
      element match {
        case childObject: MetaObject => new MetaObjectWithOrigin(childObject, SequenceSelection(this, key, index))
        case _ => element
      }
    })
    case child => child
  }

  override def hashCode(): Int = origin match {
    case Root => 1 //TODO obj.hashCode()
    case _ => origin.hashCode()
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: MetaObjectWithOrigin => origin.equals(other.origin) && (this.origin match {
      case Root => true //TODO this.obj.equals(other.obj)
      case _ => true
    })
    case _ => false
  }

  override def data2: Map[Any, Any] = obj.data.keys.map(key => (key,apply(key))).toMap
}
