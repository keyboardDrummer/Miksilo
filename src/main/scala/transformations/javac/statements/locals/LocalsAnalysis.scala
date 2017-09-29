package transformations.javac.statements.locals

import core.particles.node.Node
import core.particles.path.Path
import core.particles.Language
import transformations.javac.methods.VariablePool
import transformations.javac.statements.{StatementFlowAnalysis, StatementSkeleton}

class LocalsAnalysis(compilationState: Language, method: Node)
  extends StatementFlowAnalysis[VariablePool](compilationState: Language, method: Node) {
  
  override def updateState(state: VariablePool, node: Path): VariablePool = {
    val instances = StatementSkeleton.getState(compilationState).instances
    var newState = state
    for(entry <- instances(node.clazz).definedVariables(compilationState, node.current))
      newState = newState.add(entry._1,entry._2)
    newState
  }

  override def combineState(first: VariablePool, second: VariablePool): Option[VariablePool] = {
    if (first.typedVariables.keys == second.typedVariables.keys)
      return None

    Some(new VariablePool(first.state, first.typedVariables.filterKeys(second.typedVariables.contains)))
  }
}
