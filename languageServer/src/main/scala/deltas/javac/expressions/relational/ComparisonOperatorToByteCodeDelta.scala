package deltas.javac.expressions.relational
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.expression.relational.ComparisonOperatorDelta.ComparisonOperator
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

trait ComparisonOperatorToByteCodeDelta extends ConvertsToByteCodeDelta {

  def instruction: Node

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val operator: ComparisonOperator[NodePath] = expression
    val firstInstructions = toInstructions(operator.left)
    val secondInstructions = toInstructions(operator.right)
    firstInstructions ++ secondInstructions ++ Seq(instruction)
  }
}
