package transformations.javac.statements

import core.particles._
import transformations.javac.methods.MethodC
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](state: CompilationState, method: MetaObject)
  extends DataFlowAnalysis[Origin, State]
{
  val instances = StatementSkeleton.getState(state).instances
  val labels = getLabels

  def getLabels: Map[Any, Origin] = {
    val statements = MethodC.getMethodBody[Origin](new Root(method))
    statements.flatMap(statement => instances(statement.clazz).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: Origin): Set[Origin] = {
    instances(node.clazz).getNextStatements(node, labels)
  }
}
