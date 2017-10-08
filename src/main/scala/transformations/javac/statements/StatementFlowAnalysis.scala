package transformations.javac.statements

import core.particles._
import core.particles.node.Node
import core.particles.path.{Path, PathRoot}
import transformations.javac.methods.MethodDelta
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](state: Language, method: Node)
  extends DataFlowAnalysis[Path, State]
{
  val instances = StatementSkeleton.getRegistry(state).instances
  val labels = getLabels

  def getLabels: Map[Any, Path] = {
    val statements: Seq[Path] = PathRoot(method).selfAndDescendants.filter(node => instances.contains(node.clazz))
    statements.flatMap(statement => instances(statement.clazz).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: Path): Set[Path] = {
    instances(node.clazz).getNextStatements(node, labels)
  }
}
