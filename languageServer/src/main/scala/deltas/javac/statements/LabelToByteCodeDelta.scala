package deltas.javac.statements

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.bytecode.simpleBytecode.InferredStackFrames
import deltas.javac.expressions.ConvertsToByteCodeDelta
import deltas.statement.LabelStatementDelta
import deltas.statement.LabelStatementDelta.getName

object LabelToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(InferredStackFrames.label(getName(statement.current)))
  }

  override def shape: NodeShape = LabelStatementDelta.Shape

  override def description: String = "Converts a label statement to bytecode"

  override def dependencies: Set[Contract] = Set(LabelStatementDelta, InferredStackFrames)
}
