package core.particles

trait Path extends MetaLikeGen[Path] {
  val current: MetaObject //TODO rename to current
  
  def parentOption: Option[Path]
  def ancestors: Stream[Path] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def clazz = current.clazz
  def apply(key: Any) = get(key).get
  def get(key: Any): Option[Any] = current.data.get(key).map {
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
  
  override def data2: Map[Any, Any] = current.data.keys.map(key => (key,apply(key))).toMap
}

trait OriginWithParent extends Path {
  def parent: Path
  def replaceWith(replacement: MetaObject)
}

case class Root(current: MetaObject) extends Path with Key{
  override def parentOption: Option[Path] = None

  override def hashCode(): Int = 1 //obj.hashCode

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Root] //&& obj.equals..
}

case class Selection(parent: Path, field: Any) extends OriginWithParent {
  val current = parent.current(field).asInstanceOf[MetaObject]
  override def parentOption: Option[Path] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Selection => other.parent.equals(parent) && other.field.equals(field)
    case _ => false
  }

  override def replaceWith(replacement: MetaObject): Unit = parent(field) = replacement //TODO hier hoort nog .obj. Hoezo compiled dit?
}

case class SequenceSelection(parent: Path, field: Any, index: Int) extends OriginWithParent
{
  val current = parent.current(field).asInstanceOf[Seq[MetaObject]](index)
  def sequence: Seq[Path] = parent(field).asInstanceOf[Seq[Path]]
  def next = sequence(index + 1)
  def hasNext = sequence.length > (index + 1)
  def current = sequence(index)
  def replaceWith(replacements: Seq[MetaObject]) = {
    val originalSequence = parent.current.data(field).asInstanceOf[Seq[Path]]
    val newSequence = originalSequence.take(index) ++ replacements ++ originalSequence.drop(index + 1)
    parent.current.data(field) = newSequence
  }

  override def parentOption: Option[Path] = Some(parent)

  override def hashCode(): Int = parent.hashCode() * field.hashCode() * index

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: SequenceSelection => other.parent.equals(parent) && other.field.equals(field) && other.index == index
    case _ => false
  }


  override def replaceWith(replacement: MetaObject): Unit = replaceWith(Seq(replacement))
}

object Path {
  implicit def toSimpleObject(withOrigin: Path): MetaObject = withOrigin.current
}
