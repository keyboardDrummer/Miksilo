package miksilo.modularLanguages.deltas.javac.expressions.additive

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.SubtractIntegerDelta
import miksilo.modularLanguages.deltas.expression.LeftAssociativeBinaryOperatorDelta
import miksilo.modularLanguages.deltas.expression.additive.SubtractionDelta
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object SubtractionToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    val subtraction: LeftAssociativeBinaryOperatorDelta.BinaryOperator[NodePath] = expression
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(subtraction.left)
    val secondInstructions = toInstructions(subtraction.right)
    firstInstructions ++ secondInstructions ++ Seq(SubtractIntegerDelta.subtractInteger)
  }

  override def shape = SubtractionDelta.Shape

  override def description = "Converts - to bytecode."

  override def dependencies = Set(SubtractIntegerDelta, SubtractionDelta)
}
