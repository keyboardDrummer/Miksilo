package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.InferredStackFrames
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.statement.LabelStatementDelta
import miksilo.modularLanguages.deltas.statement.LabelStatementDelta.getName

object LabelToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(InferredStackFrames.label(getName(statement.current)))
  }

  override def shape: NodeShape = LabelStatementDelta.Shape

  override def description: String = "Converts a label statement to bytecode"

  override def dependencies: Set[Contract] = Set(LabelStatementDelta, InferredStackFrames)
}
