package deltas.javac.expressions.additive

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.integers.SubtractIntegerDelta
import deltas.expression.additive.SubtractionDelta
import deltas.expression.additive.SubtractionDelta.Subtraction
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object SubtractionToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    val subtraction: Subtraction[NodePath] = expression
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(subtraction.left)
    val secondInstructions = toInstructions(subtraction.right)
    firstInstructions ++ secondInstructions ++ Seq(SubtractIntegerDelta.subtractInteger)
  }

  override def shape = SubtractionDelta.Shape

  override def description = "Converts - to bytecode."

  override def dependencies = Set(SubtractIntegerDelta, SubtractionDelta)
}
