package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.LabelledLocations
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.statement.GotoStatementDelta
import miksilo.modularLanguages.deltas.statement.GotoStatementDelta.getTarget

object GotoToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Convert a goto statement to bytecode"

  override def dependencies: Set[Contract] = Set(GotoStatementDelta, LabelledLocations)

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(LabelledLocations.goTo(getTarget(statement.current)))
  }

  override def shape: NodeShape = GotoStatementDelta.Shape
}
