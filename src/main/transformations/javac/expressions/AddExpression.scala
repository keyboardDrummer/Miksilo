package transformations.javac.expressions

import core.grammar.FailureG
import core.transformation.{GrammarCatalogue, GrammarTransformation, MetaObject, TransformationState}

import scala.collection.mutable

object AddExpression extends GrammarTransformation {

  object ExpressionGrammar

  class ExpressionTransformations extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  def getExpressionToLines(state: TransformationState) =
    state.data.getOrElseUpdate(this, new ExpressionTransformations()).asInstanceOf[ExpressionTransformations]

  override def transformGrammars(grammars: GrammarCatalogue) {
    grammars.create(ExpressionGrammar, FailureG)
  }
}
