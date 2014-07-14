package transformations.javac.statements

import core.transformation._
import transformations.javac.expressions.ExpressionC

import scala.collection.mutable

object StatementC extends GrammarTransformation {

  object StatementGrammar

  override def dependencies: Set[ProgramTransformation] = Set(ExpressionC)

  class StatementTransformations extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  def getToInstructions(state: TransformationState): MetaObject => Seq[MetaObject] = {
    statement => {
      val statementTransformation = getStatementToLines(state).get(statement.clazz)
      val expressionTransformation = ExpressionC.getExpressionToLines(state).get(statement.clazz)
      statementTransformation.orElse(expressionTransformation).get(statement)
    }
  }

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit
  = delimiters ++= Seq(";")

  def getStatementToLines(state: TransformationState): StatementTransformations =
    state.data.getOrElseUpdate(this, new StatementTransformations()).asInstanceOf[StatementTransformations]

  override def transformGrammars(grammars: GrammarCatalogue) {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val expressionStatement = expression <~ ";"
    grammars.create(StatementGrammar, expressionStatement)
  }
}
