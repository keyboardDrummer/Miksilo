package transformations.javac.statements

import core.particles._
import core.particles.path.Root
import transformations.javac.methods.MethodC
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](state: CompilationState, method: MetaObject)
  extends DataFlowAnalysis[Path, State]
{
  val instances = StatementSkeleton.getState(state).instances
  val labels = getLabels

  def getLabels: Map[Any, Path] = {
    val statements = MethodC.getMethodBody[Path](new Root(method))
    statements.flatMap(statement => instances(statement.clazz).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: Path): Set[Path] = {
    instances(node.clazz).getNextStatements(node, labels)
  }
}
