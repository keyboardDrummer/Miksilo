package transformations.javac.statements

import core.particles._
import core.particles.node.Node
import core.particles.path.{Path, PathRoot}
import transformations.javac.methods.MethodC
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](state: CompilationState, method: Node)
  extends DataFlowAnalysis[Path, State]
{
  val instances = StatementSkeleton.getState(state).instances
  val labels = getLabels

  def getLabels: Map[Any, Path] = {
    val statements = MethodC.getMethodBody[Path](PathRoot(method))
    statements.flatMap(statement => instances(statement.clazz).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: Path): Set[Path] = {
    instances(node.clazz).getNextStatements(node, labels)
  }
}
