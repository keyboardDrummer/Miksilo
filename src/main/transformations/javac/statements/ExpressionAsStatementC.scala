package transformations.javac.statements

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, CompilationState}
import transformations.bytecode.coreInstructions.{Pop2C, PopC}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.types.TypeSkeleton

object ExpressionAsStatementC extends StatementInstance {

  object ExpressionAsStatementKey

  object ExpressionKey

  def asStatement(expression: MetaObject) = new MetaObject(ExpressionAsStatementKey, ExpressionKey -> expression)

  override val key: AnyRef = ExpressionAsStatementKey

  override def toByteCode(statement: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val expression = statement(ExpressionKey).asInstanceOf[MetaObject]
    val _type = ExpressionSkeleton.getType(state)(expression)
    val extra = TypeSkeleton.getTypeSize(_type, state) match {
      case 0 => Seq.empty
      case 1 => Seq(PopC.pop)
      case 2 => Seq(Pop2C.pop2)
    }
    ExpressionSkeleton.getToInstructions(state)(expression) ++ extra
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionAsStatement = (expressionGrammar <~ ";") ^^ parseMap(ExpressionAsStatementKey, ExpressionKey)
    statementGrammar.addOption(expressionAsStatement)
  }

  override def description: String = "Enables using an expression as a statement."
}
