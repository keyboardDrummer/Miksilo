package transformations.javac.statements

import core.transformation._
import transformations.javac.expressions.{ExpressionC, MissingToInstructionsFor}

import scala.collection.mutable


object StatementC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  def getToInstructions(state: TransformationState): MetaObject => Seq[MetaObject] = {
    statement => {
      val statementTransformation = getStatementToLines(state).get(statement.clazz)
      val expressionTransformation = ExpressionC.getExpressionToLines(state).get(statement.clazz)
      val transformation = statementTransformation.orElse(expressionTransformation)
        .getOrElse(throw new MissingToInstructionsFor(statement.clazz))
      transformation(statement)
    }
  }

  def getStatementToLines(state: TransformationState): StatementTransformations =
    state.data.getOrElseUpdate(this, new StatementTransformations()).asInstanceOf[StatementTransformations]

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit
  = delimiters ++= Seq(";")

  override def transformGrammars(grammars: GrammarCatalogue) {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val expressionStatement = expression <~ ";"
    grammars.create(StatementGrammar, expressionStatement)
  }

  class StatementTransformations extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  object StatementGrammar

}
