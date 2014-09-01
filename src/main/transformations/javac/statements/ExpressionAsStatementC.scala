package transformations.javac.statements

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.PopC
import transformations.javac.expressions.ExpressionC
import transformations.types.VoidTypeC

object ExpressionAsStatementC extends StatementInstance {

  object ExpressionAsStatementKey

  object ExpressionKey

  def asStatement(expression: MetaObject) = new MetaObject(ExpressionAsStatementKey, ExpressionKey -> expression)

  override val key: AnyRef = ExpressionAsStatementKey

  override def toByteCode(statement: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val expression = statement(ExpressionKey).asInstanceOf[MetaObject]
    val _type = ExpressionC.getType(state)(expression)
    val extra = if (VoidTypeC.voidType != _type) Seq(PopC.pop) else Seq()
    ExpressionC.getToInstructions(state)(expression) ++ extra
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val statementGrammar = grammars.find(StatementC.StatementGrammar)
    val expressionAsStatement = expressionGrammar <~ ";" ^^ parseMap(ExpressionAsStatementKey, ExpressionKey)
    statementGrammar.orToInner(expressionAsStatement)
  }
}
