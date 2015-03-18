package core.particles

//Rename to Path.
trait Origin extends MetaLikeGen[Origin] {
  val obj: MetaObject //TODO rename to current
  
  def parentOption: Option[Origin]
  def ancestors: Stream[Origin] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def clazz = obj.clazz
  def apply(key: Any) = get(key).get
  def get(key: Any): Option[Any] = obj.data.get(key).map {
    case childObject: MetaObject => new Selection(this, key)
    case sequence: Seq[_] => sequence.indices.map(index => {
      val element = sequence(index)
      element match {
        case childObject: MetaObject => new SequenceSelection(this, key, index)
        case _ => element
      }
    })
    case child => child
  }
  
  override def data2: Map[Any, Any] = obj.data.keys.map(key => (key,apply(key))).toMap
}

trait OriginWithParent extends Origin {
  def parent: Origin
  def replaceWith(replacement: MetaObject)
}

case class Root(obj: MetaObject) extends Origin with Key{
  override def parentOption: Option[Origin] = None

  override def hashCode(): Int = 1 //obj.hashCode

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Root] //&& obj.equals..
}

case class Selection(parent: Origin, field: Any) extends OriginWithParent {
  val obj = parent.obj(field).asInstanceOf[MetaObject]
  override def parentOption: Option[Origin] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Selection => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }

  override def replaceWith(replacement: MetaObject): Unit = parent(field) = replacement //TODO hier hoort nog .obj. Hoezo compiled dit?
}

case class SequenceSelection(parent: Origin, field: Any, index: Int) extends OriginWithParent
{
  val obj = parent.obj(field).asInstanceOf[Seq[MetaObject]](index)
  def sequence: Seq[Origin] = parent(field).asInstanceOf[Seq[Origin]]
  def next = sequence(index + 1)
  def hasNext = sequence.length > (index + 1)
  def current = sequence(index)
  def replaceWith(replacements: Seq[MetaObject]) = {
    val originalSequence = parent.obj.data(field).asInstanceOf[Seq[Origin]]
    val newSequence = originalSequence.take(index) ++ replacements ++ originalSequence.drop(index + 1)
    parent.obj.data(field) = newSequence
  }

  override def parentOption: Option[Origin] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode() * index

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: SequenceSelection => other.parent.equals(parent) && other.field.equals(field) && other.index == index
    case _ => false
  }


  override def replaceWith(replacement: MetaObject): Unit = replaceWith(Seq(replacement))
}

object Origin {
  implicit def toSimpleObject(withOrigin: Origin): MetaObject = withOrigin.obj
}
