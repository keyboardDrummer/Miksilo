package deltas.javac.statements.locals

import core.deltas.node.Node
import core.deltas.path.Path
import core.deltas.{Compilation, Language}
import deltas.javac.methods.VariablePool
import deltas.javac.statements.{StatementFlowAnalysis, StatementSkeleton}

class LocalsAnalysis(compilation: Compilation, method: Node)
  extends StatementFlowAnalysis[VariablePool](compilation: Language, method: Node) {
  
  override def updateState(state: VariablePool, node: Path): VariablePool = {
    val instances = StatementSkeleton.getRegistry(compilation).instances
    var newState = state
    for(entry <- instances(node.shape).definedVariables(compilation, node.current))
      newState = newState.add(entry._1,entry._2)
    newState
  }

  override def combineState(first: VariablePool, second: VariablePool): Option[VariablePool] = {
    if (first.typedVariables.keys == second.typedVariables.keys)
      return None

    Some(VariablePool(first.state, first.typedVariables.filterKeys(second.typedVariables.contains)))
  }
}
