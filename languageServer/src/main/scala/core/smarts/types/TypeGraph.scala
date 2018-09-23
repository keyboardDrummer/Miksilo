package core.smarts.types

import core.smarts.types.objects.{ConcreteType, Type}

import scala.collection.mutable

trait TypeGraphNode
case class TypeNode(_type: Type) extends TypeGraphNode
{
  override def toString = _type.toString
}

trait TypeGraphEdge {
  def target: TypeGraphNode
}

case class SuperType(target: TypeGraphNode) extends TypeGraphEdge
{
}

class TypeGraph extends scala.collection.mutable.HashMap[TypeGraphNode, mutable.Set[TypeGraphEdge]]
{
  def areCompatible(left: Type, right: Type): Boolean = {
    val rightNode: TypeNode = TypeNode(right)
    val leftNode: TypeNode = TypeNode(left)
    isSuperType(leftNode, rightNode) || isSuperType(rightNode, leftNode)
  }

  def isSuperType(superType: TypeGraphNode, subType: TypeGraphNode): Boolean = {
    getSuperTypes(subType).contains(superType)
  }

  def getSuperTypes(_type: TypeGraphNode): Seq[TypeGraphNode] = {
    var result = List.empty[TypeGraphNode]
    val visited = mutable.Set.empty[TypeGraphNode]
    val queue = new mutable.Queue[TypeGraphNode]
    queue.enqueue(_type)
    while(queue.nonEmpty)
    {
      val element = queue.dequeue()
      if (visited.add(element))
      {
        result ::= element
        this.get(element).foreach(x => x.foreach(c => queue.enqueue(c.target)))
      }
    }
    result.reverse
  }

  def add(subType: ConcreteType, superType: ConcreteType): Unit =
  {
    add(TypeNode(subType), SuperType(TypeNode(superType)))
  }

  def add(node: TypeGraphNode, edge: TypeGraphEdge): Unit =
  {
    val edges = this.getOrElseUpdate(node, mutable.Set.empty)
    edges.add(edge)
  }
}
