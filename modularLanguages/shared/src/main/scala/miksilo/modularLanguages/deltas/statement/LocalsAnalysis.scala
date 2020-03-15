package miksilo.modularLanguages.deltas.statement

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.javac.methods.VariablePool
import miksilo.modularLanguages.deltas.javac.statements.StatementFlowAnalysis

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
