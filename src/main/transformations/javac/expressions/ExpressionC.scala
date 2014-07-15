package transformations.javac.expressions

import core.grammar.FailureG
import core.transformation._
import transformations.bytecode.LabelledJumps

import scala.collection.mutable

object ExpressionC extends GrammarTransformation {

  object ExpressionGrammar

  override def dependencies: Set[Contract] = Set(LabelledJumps)

  class ExpressionTransformations extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]] {
    def putIfEmpty(key: AnyRef, value: MetaObject => Seq[MetaObject]) = {
      if (!this.contains(key))
        put(key, value)
    }
  }

  def getToInstructions(state: TransformationState): MetaObject => Seq[MetaObject] = {
    expression => getExpressionToLines(state)(expression.clazz)(expression)
  }

  def getExpressionToLines(state: TransformationState): ExpressionTransformations =
    state.data.getOrElseUpdate(this, new ExpressionTransformations()).asInstanceOf[ExpressionTransformations]

  override def transformGrammars(grammars: GrammarCatalogue) {
    grammars.create(ExpressionGrammar, FailureG)
  }
}
