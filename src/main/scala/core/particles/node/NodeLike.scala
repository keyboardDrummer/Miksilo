package core.particles.node

import scala.collection.mutable

object NodeWrapper
{
  implicit def wrapList[TNodeWrapper](list: Seq[Node])(implicit wrap: Node => TNodeWrapper): Seq[TNodeWrapper] = list.map(n => wrap(n))
  implicit def unwrapList(list: Seq[NodeWrapper]): Seq[Node] = list.map(n => n.node)
  implicit def unwrap(wrapper: NodeWrapper): Node = wrapper.node
}

trait NodeWrapper extends Any {
  def node: Node
}

trait NodeLike {
  type Self <: NodeLike
  def get(key: Any): Option[Any]
  def apply(key: Any): Any
  def clazz: Any
  def dataView: Map[Any, Any]

  def getDescendants: List[Self] = {
    var result = List.empty[Self]
    foreach(node => result = node :: result)
    result
  }

  def foreach(transformation: Self => Unit, visited: mutable.Set[Self] = new mutable.HashSet[Self]()) = {

    transformNode(this.asInstanceOf[Self])
    def transformNode(node: Self): Unit = {
      if (!visited.add(node))
        return

      transformation(node)

      val children = node.dataView.values
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
