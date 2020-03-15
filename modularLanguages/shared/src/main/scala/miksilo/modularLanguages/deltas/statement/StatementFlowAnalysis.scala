package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.path.{NodePath, PathRoot}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.statement.ControlFlowGraph
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](language: Language, body: Node, initialState: State)
  extends DataFlowAnalysis[NodePath, State]
{
  val bodyPath = PathRoot(body)
  private val instances = ControlFlowGraph.instances.get(language)
  private val labels = getLabels
  private val controlFlowGraph = ControlFlowGraph.getControlFlowGraph(language, bodyPath, labels)

  private val rootNode: NodePath = controlFlowGraph.root.get
  addRootNode(rootNode, initialState)

  def getLabels: Map[Any, NodePath] = {
    val statements: Seq[NodePath] = bodyPath.selfAndDescendants.filter(node => instances.contains(node.shape))
    statements.flatMap(statement => instances(statement.shape).getLabels(language, statement)).toMap
  }

  override def getOutgoingNodes(node: NodePath): Set[NodePath] = {
    controlFlowGraph(node)
  }
}
