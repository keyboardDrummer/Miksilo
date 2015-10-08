package core.particles.node

import scala.collection.mutable

trait NodeLike {
  type Self <: NodeLike
  def get(key: Any): Option[Any]
  def apply(key: Any): Any
  def clazz: Any
  def dataView: Map[Any, Any]

  def transform(transformation: Self => Unit, visited: mutable.Set[Self] = new mutable.HashSet[Self]()) = {

    transformNode(this.asInstanceOf[Self])
    def transformNode(metaObject: Self): Unit = {
      if (!visited.add(metaObject))
        return

      transformation(metaObject)

      val children = metaObject.dataView.values
      for(child <- children)
      {
        child match {
          case metaObject: NodeLike =>
            transformNode(metaObject.asInstanceOf[Self])
          case sequence: Seq[_] =>
            sequence.reverse.foreach({ //TODO: the reverse is a nasty hack to decrease the chance of mutations conflicting with this iteration. Problem would occur when transforming two consecutive declarationWithInitializer's
              case metaChild: NodeLike => transformNode(metaChild.asInstanceOf[Self])
              case _ =>
            })
          case _ =>
        }
      }
    }
  }
}
