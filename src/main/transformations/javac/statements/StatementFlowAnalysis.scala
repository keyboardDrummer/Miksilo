package transformations.javac.statements

import core.particles._
import transformations.javac.methods.MethodC
import util.DataFlowAnalysis

abstract class StatementFlowAnalysis[State](state: CompilationState, method: MetaObject)
  extends DataFlowAnalysis[MetaObjectWithOrigin, State]
{
  val instances = StatementSkeleton.getState(state).instances
  val labels = getLabels

  def getLabels: Map[Any, MetaObjectWithOrigin] = {
    val statements = MethodC.getMethodBody(new MetaObjectWithOrigin(method, Root))
    statements.flatMap(statement => instances(statement.clazz).getLabels(statement)).toMap
  }

  override def getOutgoingNodes(node: MetaObjectWithOrigin): Set[MetaObjectWithOrigin] = {
    instances(node.clazz).getNextStatements(node, labels)
  }
}
