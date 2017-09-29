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
  def update(key: Any, value: Any): Unit
  def clazz: Any
  def dataView: Map[Any, Any]

  def selfAndDescendants: List[Self] = {
    var result = List.empty[Self]
    visit(node => result = node :: result)
    result
  }

  def visit(afterChildren: (Self) => Unit = _ => {},
            beforeChildren: (Self) => Boolean = _ => true,
            visited: mutable.Set[Self] = new mutable.HashSet[Self]()): Unit = {

    transformNode(this.asInstanceOf[Self])
    def transformNode(node: Self): Unit = {
      if (!visited.add(node))
        return

      if (!beforeChildren(node))
        return

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

      afterChildren(node)
    }
  }
}
