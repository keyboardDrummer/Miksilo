package miksilo.modularLanguages.deltas.javac.expressions.relational
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.expression.LeftAssociativeBinaryOperatorDelta.BinaryOperator
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

trait ComparisonOperatorToByteCodeDelta extends ConvertsToByteCodeDelta {

  def instruction: Node

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val operator: BinaryOperator[NodePath] = expression
    val firstInstructions = toInstructions(operator.left)
    val secondInstructions = toInstructions(operator.right)
    firstInstructions ++ secondInstructions ++ Seq(instruction)
  }
}
