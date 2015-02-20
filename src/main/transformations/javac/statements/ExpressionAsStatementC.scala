package transformations.javac.statements

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.{Pop2C, PopC}
import transformations.javac.expressions.ExpressionC
import transformations.types.TypeC

object ExpressionAsStatementC extends StatementInstance {

  object ExpressionAsStatementKey

  object ExpressionKey

  def asStatement(expression: MetaObject) = new MetaObject(ExpressionAsStatementKey, ExpressionKey -> expression)

  override val key: AnyRef = ExpressionAsStatementKey

  override def toByteCode(statement: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val expression = statement(ExpressionKey).asInstanceOf[MetaObject]
    val _type = ExpressionC.getType(state)(expression)
    val extra = TypeC.getTypeSize(_type, state) match {
      case 0 => Seq.empty
      case 1 => Seq(PopC.pop)
      case 2 => Seq(Pop2C.pop)
    }
    ExpressionC.getToInstructions(state)(expression) ++ extra
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val statementGrammar = grammars.find(StatementC.StatementGrammar)
    val expressionAsStatement = (expressionGrammar <~ ";") ^^ parseMap(ExpressionAsStatementKey, ExpressionKey)
    statementGrammar.addOption(expressionAsStatement)
  }
}
