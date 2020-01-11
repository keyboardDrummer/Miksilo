package deltas.statement

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.javac.methods.VariablePool
import deltas.javac.statements.StatementFlowAnalysis

class LocalsAnalysis(compilation: Compilation, body: Node, initialVariables: VariablePool)
  extends StatementFlowAnalysis[VariablePool](compilation, body, initialVariables) {

  private val instances = StatementDelta.instances.get(compilation)
  override def updateState(state: VariablePool, node: NodePath): VariablePool = {
    var newState = state
    for(entry <- instances(node.shape).definedVariables(compilation, node.current))
      newState = newState.add(entry._1,entry._2)
    newState
  }

  override def combineState(first: VariablePool, second: VariablePool): Option[VariablePool] = {
    if (first.typedVariables.keys == second.typedVariables.keys)
      return None

    Some(VariablePool(first.language, first.typedVariables.view.filterKeys(second.typedVariables.contains).toMap))
  }
}
