package core.language.node

import core.deltas.path.NodePath

import scala.collection.mutable

trait NodeLike {
  type Self <: NodeLike
  def get(key: NodeField): Option[Any]
  def apply(key: NodeField): Any
  def update(key: NodeField, value: Any): Unit
  def shape: NodeShape
  def shape_=(value: NodeShape): Unit
  def dataView: Map[NodeField, Any]
  def asPath: Option[NodePath]
  def asNode: Node

  def selfAndDescendants: List[Self] = {
    var result = List.empty[Self]
    visit(node => result = node :: result)
    result
  }

  def visitShape(shape: NodeShape): Seq[Self] = {
    selfAndDescendants.filter(p => p.shape == shape)
  }

  def visitShape(shape: NodeShape, afterChildren: (Self) => Unit): Unit = {
    visit(node => {
      if (node.shape == shape)
        afterChildren(node)
    })
  }

  def visit(afterChildren: (Self) => Unit = _ => {},
            beforeChildren: (Self) => Boolean = _ => true,
            visited: mutable.Set[Self] = new mutable.HashSet[Self]()): Unit = {

    visitNode(this.asInstanceOf[Self])
    def visitNode(node: Self): Unit = {
      if (!visited.add(node))
        return

      if (!beforeChildren(node))
        return

      val children = node.dataView.values
      for(child <- children)
        NodeLike.getNodeLikesFromValue[Self](child).foreach(visitNode)

      afterChildren(node)
    }
  }
}

object NodeLike {

  def getNodeLikesFromValue[Self <: NodeLike](value: Any): Seq[Self] = value match {
    case nodeLike: NodeLike =>
      Seq(nodeLike.asInstanceOf[Self])
    case sequence: Seq[_] =>
      sequence.reverse. //TODO: the reverse is a nasty hack to decrease the chance of mutations conflicting with this iteration. Problem would occur when transforming two consecutive declarationWithInitializer's
        collect({ case nodeLikeChild: NodeLike => nodeLikeChild.asInstanceOf[Self] })
    case _ => Seq.empty
  }

}
