package core.smarts.scopes

import core.language.SourceElement
import core.language.node.FileRange
import core.smarts.objects.{NamedDeclaration, Reference}
import core.smarts.scopes.objects.ConcreteScope

import scala.collection.mutable

trait GraphNode {
  def origin: Option[SourceElement]
}

trait GraphEdge {
  def target: GraphNode
  def traverse: Boolean
}

case class ReferenceEdge(target: ConcreteScope) extends GraphEdge
{
  override def traverse: Boolean = true
}
case class ImportEdge(target: GraphNode) extends GraphEdge {
  override def traverse: Boolean = true
}
case class DeclaresDeclaration(target: NamedDeclaration) extends GraphEdge {
  override def traverse: Boolean = true
}
case class Parent(target: ConcreteScope) extends GraphEdge {
  override def traverse: Boolean = true
}
case class DeclaresScope(target: ConcreteScope) extends GraphEdge {
  override def traverse: Boolean = false
}

/*
References are inside scopes.
Scopes can be inside other scopes.
Declarations are inside scopes.
A declaration can declare a scope.
 */
class ScopeGraph extends
{
  val nodes = mutable.Map.empty[GraphNode, mutable.Set[GraphEdge]]
  val rangeToNode = mutable.Map.empty[FileRange, GraphNode]
  val elementToNode = mutable.Map.empty[SourceElement, GraphNode]
  var declarationsPerFile = mutable.Map.empty[String, mutable.HashSet[NamedDeclaration]]

  def findDeclaration(location: SourceElement): Option[NamedDeclaration] = {
    val declarations = for {
      elementRange <- location.fileRange
      result <- rangeToNode.get(elementRange)
    } yield result

    declarations.collect({ case n: NamedDeclaration => n })
  }

  def findReference(location: SourceElement): Option[Reference] = {
    val references = for {
      elementRange <- location.fileRange
      result <- rangeToNode.get(elementRange)
    } yield result

    references.collect({ case n: Reference => n })
  }

  def addImport(currentScope: ConcreteScope, importedScope: ConcreteScope): Unit = addEdge(currentScope, ImportEdge(importedScope))

  def resolveScope(importedModule: NamedDeclaration): ConcreteScope = {
    val reachableNodes = depthFirst(importedModule).collect({case d:ConcreteScope => d})
    if (reachableNodes.nonEmpty)
    {
      return reachableNodes.head
    }
    null
  }

  def addReference(reference: Reference, currentScope: ConcreteScope): Unit = addEdge(reference, ReferenceEdge(currentScope))

  def resolveWithoutNameCheck(reference: Reference): Seq[NamedDeclaration] = {
    val reachableNodes = depthFirst(reference).collect({case d:NamedDeclaration => d})

    if (reachableNodes.isEmpty)
      return Seq.empty

    reachableNodes
  }

  def resolve(reference: Reference): Seq[NamedDeclaration] = {
    resolveWithoutNameCheck(reference).filter(d => d.name == reference.name)
  }

  case class DebugNode(node: GraphNode, graph: ScopeGraph) {
    def next: Seq[DebugNode] = {
      graph.nodes(node).map(n => DebugNode(n.target, graph)).toSeq
    }
  }

  def debug(node: GraphNode): DebugNode = DebugNode(node, this)

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
        nodes.get(element).foreach(x => x.filter(c => c.traverse).foreach(c => queue.enqueue(c.target)))
      }
    }
    result.reverse
  }

  def parent(child: ConcreteScope, parent: ConcreteScope): Unit = addEdge(child, Parent(parent))
  def declareDeclaration(inside: ConcreteScope, declaration: NamedDeclaration): Unit = addEdge(inside, DeclaresDeclaration(declaration))
  def declareScope(declaration: NamedDeclaration, scope: ConcreteScope): Unit = addEdge(declaration, DeclaresScope(scope))

  def addEdge(source: GraphNode, edge: GraphEdge): Boolean =
  {
    source.origin.foreach(addNode(source, _))
    edge.target.origin.foreach(addNode(edge.target, _))
    val edges = nodes.getOrElseUpdate(source, mutable.Set.empty)
    edges.add(edge)
  }

  private def addNode(node: GraphNode, element: SourceElement) {
    elementToNode(element) = node
    element.fileRange.foreach(position => rangeToNode(position) = node)
    node match {
      case declaration: NamedDeclaration => element.uriOption.foreach(file => {
          val declarations = declarationsPerFile.getOrElseUpdate(file, new mutable.HashSet[NamedDeclaration]())
          declarations.add(declaration)
        })
      case _ =>
    }
  }

}
