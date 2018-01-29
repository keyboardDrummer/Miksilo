package core.nabl.scopes

import core.language.SourceElement
import core.nabl.objects.{NamedDeclaration, Reference}
import core.nabl.scopes.objects.ConcreteScope

import scala.collection.mutable

trait GraphNode
case class ScopeNode(scope: ConcreteScope) extends GraphNode
{
  override def toString: String = scope.toString
}
case class ReferenceNode(reference: Reference) extends GraphNode
{
  override def toString: String = reference.toString
}
case class DeclarationNode(declaration: NamedDeclaration) extends GraphNode
{
  override def toString: String = declaration.toString
}


trait GraphEdge {
  def target: GraphNode
  def traverse: Boolean
}
case class ReferenceEdge(target: ScopeNode) extends GraphEdge
{
  override def traverse: Boolean = true
}
case class ImportEdge(target: ScopeNode) extends GraphEdge {
  override def traverse: Boolean = true
}
case class DeclaresDeclaration(target: DeclarationNode) extends GraphEdge {
  override def traverse: Boolean = true
}
case class Parent(target: ScopeNode) extends GraphEdge {
  override def traverse: Boolean = true
}
case class DeclaresScope(target: ScopeNode) extends GraphEdge {
  override def traverse: Boolean = false
}

class ScopeGraph extends scala.collection.mutable.HashMap[GraphNode, mutable.Set[GraphEdge]]
{
  def findReference(location: SourceElement): Reference = ???
  def resolveLocation(location: SourceElement): SourceElement = resolve(findReference(location)).origin

  def addImport(currentScope: ConcreteScope, importedScope: ConcreteScope): Unit = add(ScopeNode(currentScope), ImportEdge(ScopeNode(importedScope)))

  def resolveScope(importedModule: NamedDeclaration): ConcreteScope = {
    val reachableNodes = depthFirst(DeclarationNode(importedModule)).collect({case d:ScopeNode => d})
    if (reachableNodes.nonEmpty)
    {
      return reachableNodes.head.scope
    }
    null
  }

  def addReference(reference: Reference, currentScope: ConcreteScope): Unit = add(ReferenceNode(reference), ReferenceEdge(ScopeNode(currentScope)))

  def resolve(reference: Reference): NamedDeclaration = {
    val reachableNodes = depthFirst(ReferenceNode(reference)).collect({case d:DeclarationNode => d}).
      filter(d => d.declaration.name == reference.name)
    if (reachableNodes.nonEmpty)
    {
      return reachableNodes.head.declaration
    }
    null
  }

  def depthFirst(root: GraphNode): Seq[GraphNode] = {
    var result = List.empty[GraphNode]
    val visited = mutable.Set.empty[GraphNode]
    val queue = new mutable.Queue[GraphNode]
    queue.enqueue(root)
    while(queue.nonEmpty)
    {
      val element = queue.dequeue()
      if (visited.add(element))
      {
        result ::= element
        this.get(element).foreach(x => x.filter(c => c.traverse).foreach(c => queue.enqueue(c.target)))
      }
    }
    result.reverse
  }

  def parent(child: ConcreteScope, parent: ConcreteScope): Unit = add(ScopeNode(child), Parent(ScopeNode(parent)))
  def declareDeclaration(inside: ConcreteScope, declaration: NamedDeclaration): Unit = add(ScopeNode(inside), DeclaresDeclaration(DeclarationNode(declaration)))
  def declareScope(declaration: NamedDeclaration, scope: ConcreteScope): Unit = add(DeclarationNode(declaration), DeclaresScope(ScopeNode(scope)))

  def add(node: GraphNode, edge: GraphEdge): Boolean =
  {
    val edges = this.getOrElseUpdate(node, mutable.Set.empty)
    edges.add(edge)
  }
}
