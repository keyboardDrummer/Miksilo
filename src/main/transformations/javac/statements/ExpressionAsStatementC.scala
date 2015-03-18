package transformations.javac.statements

import core.particles.grammars.GrammarCatalogue
import core.particles.{MetaLike, Path$, CompilationState, MetaObject}
import transformations.bytecode.coreInstructions.{Pop2C, PopC}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.types.TypeSkeleton

object ExpressionAsStatementC extends StatementInstance {

  object ExpressionAsStatementKey

  object ExpressionKey

  def asStatement(expression: MetaObject) = new MetaObject(ExpressionAsStatementKey, ExpressionKey -> expression)

  override val key: AnyRef = ExpressionAsStatementKey

  override def toByteCode(statement: Path, state: CompilationState): Seq[MetaObject] = {
    val expression = getExpression(statement)
    val _type = ExpressionSkeleton.getType(state)(expression)
    val extra = TypeSkeleton.getTypeSize(_type, state) match {
      case 0 => Seq.empty
      case 1 => Seq(PopC.pop)
      case 2 => Seq(Pop2C.pop2)
    }
    ExpressionSkeleton.getToInstructions(state)(expression) ++ extra
  }

  def getExpression[T <: MetaLike](statement: T): T = {
    statement(ExpressionKey).asInstanceOf[T]
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionAsStatement = (expressionGrammar <~ ";") ^^ parseMap(ExpressionAsStatementKey, ExpressionKey)
    statementGrammar.addOption(expressionAsStatement)
  }

  override def description: String = "Enables using an expression as a statement."

}
