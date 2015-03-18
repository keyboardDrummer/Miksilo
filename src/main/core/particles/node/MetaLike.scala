package core.particles.node

import scala.collection.mutable

trait MetaLike {

  def get(key: Any): Option[Any]
  def apply(key: Any): Any
  def clazz: Any
  def dataView: Map[Any, Any]

  def transform[T <: MetaLike](transformation: T => Unit, visited: mutable.Set[T] = new mutable.HashSet[T]()) = {

    transformNode(this.asInstanceOf[T])
    def transformNode(metaObject: T): Unit = {
      if (!visited.add(metaObject))
        return

      transformation(metaObject)

      val children = metaObject.dataView.values
      for(child <- children)
      {
        child match {
          case metaObject: MetaLike =>
            transformNode(metaObject.asInstanceOf[T])
          case sequence: Seq[_] =>
            sequence.reverse.foreach({ //TODO: the reverse is a nasty hack to decrease the chance of mutations conflicting with this iteration. Problem would occur when transforming two consecutive declarationWithInitializer's
              case metaChild: MetaLike => transformNode(metaChild.asInstanceOf[T])
              case _ =>
            })
          case _ =>
        }
      }
    }
  }
}
