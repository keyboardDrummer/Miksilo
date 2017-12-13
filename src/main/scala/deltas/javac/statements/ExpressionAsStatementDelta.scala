package deltas.javac.statements

import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import core.deltas.{Compilation, Language}
import deltas.bytecode.coreInstructions.{Pop2Delta, PopDelta}
import deltas.javac.expressions.ExpressionSkeleton
import deltas.bytecode.types.TypeSkeleton

object ExpressionAsStatementDelta extends StatementInstance {

  object Shape extends NodeShape

  object Expression extends NodeField

  def create(expression: Node): Node = new Node(Shape, Expression -> expression)

  override val key = Shape

  override def toByteCode(statement: Path, compilation: Compilation): Seq[Node] = {
    val expression = getExpression(statement)
    val _type = ExpressionSkeleton.getType(compilation)(expression)
    val extra = TypeSkeleton.getTypeSize(_type, compilation) match {
      case 0 => Seq.empty
      case 1 => Seq(PopDelta.pop)
      case 2 => Seq(Pop2Delta.pop2)
    }
    ExpressionSkeleton.getToInstructions(compilation)(expression) ++ extra
  }

  def getExpression[T <: NodeLike](statement: T): T = {
    statement(Expression).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val expressionAsStatement = expressionGrammar.as(Expression) ~< ";" asNode Shape
    statementGrammar.addOption(expressionAsStatement)
  }

  override def description: String = "Enables using an expression as a statement."

}
