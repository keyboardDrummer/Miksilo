package deltas.javac.expressions.relational

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

trait ComparisonOperatorToByteCodeDelta extends ConvertsToByteCodeDelta {

  def instruction: Node

  val base: ComparisonOperatorDelta

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val greaterThan = new base.ComparisonOperator[NodePath](expression)
    val firstInstructions = toInstructions(greaterThan.left)
    val secondInstructions = toInstructions(greaterThan.right)
    firstInstructions ++ secondInstructions ++ Seq(instruction)
  }
}
