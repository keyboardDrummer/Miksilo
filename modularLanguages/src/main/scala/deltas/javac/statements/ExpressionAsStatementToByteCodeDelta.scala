package deltas.javac.statements

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.bytecode.coreInstructions.{Pop2Delta, PopDelta}
import deltas.bytecode.types.TypeSkeleton
import deltas.expression.ExpressionDelta
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.statements.ExpressionAsStatementDelta.getExpression

object ExpressionAsStatementToByteCodeDelta extends ConvertsToByteCodeDelta  {

  override def description: String = "Transforms an expression statement into bytecode "

  override def dependencies: Set[Contract] = Set(ExpressionAsStatementDelta, PopDelta, Pop2Delta)

  override def shape: NodeShape = ExpressionAsStatementDelta.Shape

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    val expression = getExpression(statement)
    val _type = ExpressionDelta.cachedNodeType(compilation, expression)
    val extra = TypeSkeleton.getTypeSize(_type, compilation) match {
      case 0 => Seq.empty
      case 1 => Seq(PopDelta.pop)
      case 2 => Seq(Pop2Delta.pop2)
    }
    ToByteCodeSkeleton.getToInstructions(compilation)(expression) ++ extra
  }
}
