package deltas.javac.statements

import core.deltas._
import core.deltas.path.NodePath
import core.language.node.Node
import core.language.{Compilation, Language}

object ControlFlowGraph {
  val empty = ControlFlowGraph(None, Map.empty, Set.empty)
  def singleton(value: NodePath) = ControlFlowGraph(Some(value), Map.empty, Set(value))
}

case class ControlFlowGraph(root: Option[NodePath], edges: Map[NodePath, Set[NodePath]], leafs: Set[NodePath]) {

  def apply(node: NodePath): Set[NodePath] = edges.getOrElse(node, Set.empty)

  def sequence(second: ControlFlowGraph): ControlFlowGraph = {
    if (root.isEmpty)
      return second

    if (second.root.isEmpty)
      return this

    val newEdges = leafs.toSeq.map(leaf => leaf -> Set(second.root.get)).toMap
    ControlFlowGraph(root, safeEdgeMerge(edges, newEdges) ++ second.edges, second.leafs)
  }

  def parallel(second: ControlFlowGraph): ControlFlowGraph = {
    if (root != second.root) {
      throw new IllegalArgumentException("Can only put control flow graphs in parallel that have the same root.")
    }

    ControlFlowGraph(root, safeEdgeMerge(edges, second.edges), leafs ++ second.leafs)
  }

  def safeEdgeMerge(edges: Map[NodePath, Set[NodePath]], edges2: Map[NodePath, Set[NodePath]]): Map[NodePath, Set[NodePath]] = {
    (edges.keySet ++ edges2.keySet).
      map(key => (key, edges.getOrElse(key, Set.empty) ++ edges2.getOrElse(key, Set.empty))).
      toMap
  }
}

trait ByteCodeStatementInstance extends DeltaWithGrammar with HasShape {

  override def inject(language: Language): Unit = {
    ByteCodeStatementSkeleton.instances.add(language, this)
    super.inject(language)
  }

  def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node]

  case class SequenceDoesNotEndInJump(sequence: Seq[Node]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }

  def getControlFlowGraph(language: Language, statement: NodePath, labels: Map[Any, NodePath]): ControlFlowGraph = //TODO move to generic HasControlFlowInstance
    ControlFlowGraph.singleton(statement)

  /**
    * Returns a map of labels to statements. Can be used for jumping to a particular statement based on that label.
    */
  def getLabels(language: Language, obj: NodePath): Map[Any, NodePath] = Map.empty

  def definedVariables(compilation: Compilation, obj: Node): Map[String, Node] = Map.empty
}