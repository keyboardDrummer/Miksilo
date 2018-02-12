package core.language.node

import core.deltas.path.NodePath
import core.language.SourceElement

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

  def getLocation(field: NodeField): SourceElement

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

    transformNode(this.asInstanceOf[Self])
    def transformNode(node: Self): Unit = {
      if (!visited.add(node))
        return

      if (!beforeChildren(node))
        return

      val children = node.dataView.values
      System.out.append("")
      for(child <- children)
      {
        child match {
          case nodeLike: NodeLike =>
            transformNode(nodeLike.asInstanceOf[Self])
          case sequence: Seq[_] =>
            sequence.reverse.foreach({ //TODO: the reverse is a nasty hack to decrease the chance of mutations conflicting with this iteration. Problem would occur when transforming two consecutive declarationWithInitializer's
              case nodeLikeChild: NodeLike => transformNode(nodeLikeChild.asInstanceOf[Self])
              case _ =>
            })
          case _ =>
        }
      }

      afterChildren(node)
    }
  }
}
