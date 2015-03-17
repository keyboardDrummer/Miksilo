package transformations.javac.statements.locals

import core.particles.{MetaObjectWithOrigin, MetaObject, CompilationState}
import transformations.javac.methods.VariablePool
import transformations.javac.statements.{StatementSkeleton, StatementFlowAnalysis}

class LocalsAnalysis(compilationState: CompilationState, method: MetaObject)
  extends StatementFlowAnalysis[VariablePool](compilationState: CompilationState, method: MetaObject) {
  
  override def updateState(state: VariablePool, node: MetaObjectWithOrigin): VariablePool = {
    val instances = StatementSkeleton.getState(compilationState).instances
    var newState = state
    for(entry <- instances(node.clazz).definedVariables(compilationState, node.obj))
      newState = newState.add(entry._1,entry._2)
    newState
  }

  override def combineState(first: VariablePool, second: VariablePool): Option[VariablePool] = {
    if (first.typedVariables.keys == second.typedVariables.keys)
      return None

    Some(new VariablePool(first.state, first.typedVariables.filterKeys(second.typedVariables.contains)))
  }
}
