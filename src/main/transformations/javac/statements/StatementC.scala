package transformations.javac.statements

import core.grammar.FailureG
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.javac.expressions.ExpressionC
import transformations.javac.expressions.ExpressionC.MissingToInstructionsFor

import scala.collection.mutable


object StatementC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  def getToInstructions(state: TransformationState): MetaObject => Seq[MetaObject] = {
    statement => {
      val statementTransformation = getStatementToLines(state).get(statement.clazz)
      val transformation = statementTransformation.getOrElse(throw new MissingToInstructionsFor(statement.clazz))
      transformation(statement)
    }
  }

  def getStatementToLines(state: TransformationState): StatementTransformations =
    state.data.getOrElseUpdate(this, new StatementTransformations()).asInstanceOf[StatementTransformations]

  override def transformGrammars(grammars: GrammarCatalogue) {
    grammars.create(StatementGrammar, FailureG)
  }

  class StatementTransformations extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  object StatementGrammar

}
