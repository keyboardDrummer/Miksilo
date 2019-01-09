package deltas.javac.expressions.prefix

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.extraBooleanInstructions.NotInstructionDelta
import deltas.expression.prefix.LogicalNotDelta
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object LogicalNotToByteCode extends ConvertsToByteCodeDelta {

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    ToByteCodeSkeleton.getToInstructions(compilation)(expression) ++ Seq(NotInstructionDelta.not)
  }

  override def description = "Converts logical not to bytecode"

  override def shape = LogicalNotDelta.Shape

  override def dependencies = Set(LogicalNotDelta, NotInstructionDelta)
}
