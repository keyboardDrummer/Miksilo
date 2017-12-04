package core.deltas.node

import scala.collection.mutable

trait NodeLike {
  type Self <: NodeLike
  def get(key: NodeField): Option[Any]
  def apply(key: NodeField): Any
  def update(key: NodeField, value: Any): Unit
  def clazz: NodeClass
  def clazz_=(value: NodeClass): Unit
  def dataView: Map[NodeField, Any]

  def selfAndDescendants: List[Self] = {
    var result = List.empty[Self]
    visit(node => result = node :: result)
    result
  }

  def visitClass(clazz: NodeClass): Seq[Self] = {
    selfAndDescendants.filter(p => p.clazz == clazz)
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
