package core.particles.path

import core.particles.node.{MetaObject, MetaLikeGen}

object Path {
  implicit def toSimpleObject(withOrigin: Path): MetaObject = withOrigin.current
}

trait Path extends MetaLikeGen[Path] {
  val current: MetaObject

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

  override def dataView: Map[Any, Any] = current.data.keys.map(key => (key,apply(key))).toMap
}