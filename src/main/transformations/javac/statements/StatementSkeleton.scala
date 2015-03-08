package transformations.javac.statements

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.expressions.ExpressionSkeleton.MissingToInstructionsFor

import scala.collection.mutable


object StatementSkeleton extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

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
    grammars.create(StatementGrammar)
  }

  class StatementTransformations extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  object StatementGrammar

  override def description: String = "Defines the concept of a statement."
}
