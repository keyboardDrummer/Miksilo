package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{Pop2Delta, PopDelta}
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import miksilo.modularLanguages.deltas.javac.statements.ExpressionAsStatementDelta.getExpression

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
