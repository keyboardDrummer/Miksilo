package deltas.javac.statements.locals

import core.particles.node.Node
import core.particles.path.Path
import core.particles.{Compilation, Language}
import deltas.javac.methods.VariablePool
import deltas.javac.statements.{StatementFlowAnalysis, StatementSkeleton}

class LocalsAnalysis(compilation: Compilation, method: Node)
  extends StatementFlowAnalysis[VariablePool](compilation: Language, method: Node) {
  
  override def updateState(state: VariablePool, node: Path): VariablePool = {
    val instances = StatementSkeleton.getRegistry(compilation).instances
    var newState = state
    for(entry <- instances(node.clazz).definedVariables(compilation, node.current))
      newState = newState.add(entry._1,entry._2)
    newState
  }

  override def combineState(first: VariablePool, second: VariablePool): Option[VariablePool] = {
    if (first.typedVariables.keys == second.typedVariables.keys)
      return None

    Some(VariablePool(first.state, first.typedVariables.filterKeys(second.typedVariables.contains)))
  }
}
