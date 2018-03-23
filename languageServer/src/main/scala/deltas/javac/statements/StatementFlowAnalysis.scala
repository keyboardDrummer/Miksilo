package deltas.javac.statements

import core.language.node.Node
import core.deltas.path.{NodePath, PathRoot}
import core.language.Language
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](language: Language, method: Node)
  extends DataFlowAnalysis[NodePath, State]
{
  val instances = StatementSkeleton.instances.get(language)
  val labels = getLabels

  def getLabels: Map[Any, NodePath] = {
    val statements: Seq[NodePath] = PathRoot(method).selfAndDescendants.filter(node => instances.contains(node.shape))
    statements.flatMap(statement => instances(statement.shape).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: NodePath): Set[NodePath] = {
    instances(node.shape).getNextStatements(node, labels)
  }
}
