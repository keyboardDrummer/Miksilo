package deltas.javac.statements

import core.deltas.node.Node
import core.deltas.path.{NodePath, NodePathRoot}
import core.language.Language
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](language: Language, method: Node)
  extends DataFlowAnalysis[NodePath, State]
{
  val instances = StatementSkeleton.getRegistry(language).instances
  val labels = getLabels

  def getLabels: Map[Any, NodePath] = {
    val statements: Seq[NodePath] = NodePathRoot(method).selfAndDescendants.filter(node => instances.contains(node.shape))
    statements.flatMap(statement => instances(statement.shape).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: NodePath): Set[NodePath] = {
    instances(node.shape).getNextStatements(node, labels)
  }
}
