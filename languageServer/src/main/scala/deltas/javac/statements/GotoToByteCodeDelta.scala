package deltas.javac.statements

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.bytecode.simpleBytecode.LabelledLocations
import deltas.javac.expressions.ConvertsToByteCodeDelta
import deltas.statement.GotoStatementDelta
import deltas.statement.GotoStatementDelta.getTarget

object GotoToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(LabelledLocations.goTo(getTarget(statement.current)))
  }

  override def shape: NodeShape = GotoStatementDelta.Shape

  override def description: String = "Convert a goto statement to bytecode"

  override def dependencies: Set[Contract] = Set(GotoStatementDelta, LabelledLocations)
}
