package deltas.javac.statements

import core.deltas.node.Node
import core.deltas.path.{Path, PathRoot}
import core.language.Language
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](language: Language, method: Node)
  extends DataFlowAnalysis[Path, State]
{
  val instances = StatementSkeleton.getRegistry(language).instances
  val labels = getLabels

  def getLabels: Map[Any, Path] = {
    val statements: Seq[Path] = PathRoot(method).selfAndDescendants.filter(node => instances.contains(node.shape))
    statements.flatMap(statement => instances(statement.shape).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: Path): Set[Path] = {
    instances(node.shape).getNextStatements(node, labels)
  }
}
