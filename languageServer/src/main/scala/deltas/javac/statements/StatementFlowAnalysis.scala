package deltas.javac.statements

import core.language.node.Node
import core.deltas.path.{NodePath, PathRoot}
import core.language.Language
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](language: Language, method: Node, methodBody: NodePath, initialState: State)
  extends DataFlowAnalysis[NodePath, State]
{
  private val instances = ByteCodeStatementSkeleton.instances.get(language)
  private val labels = getLabels
  private val controlFlowGraph = ByteCodeStatementSkeleton.getInstance(language, methodBody).
      getControlFlowGraph(language, methodBody, labels)

  private val rootNode: NodePath = controlFlowGraph.root.get
  states.put(rootNode, initialState)
  nodeQueue.enqueue(rootNode)

  def getLabels: Map[Any, NodePath] = {
    val statements: Seq[NodePath] = PathRoot(method).selfAndDescendants.filter(node => instances.contains(node.shape))
    statements.flatMap(statement => instances(statement.shape).getLabels(language, statement)).toMap
  }

  override def getOutgoingNodes(node: NodePath): Set[NodePath] = {
    controlFlowGraph(node)
  }
}
