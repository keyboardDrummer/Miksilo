package transformations.javac.expressions

import core.exceptions.CompilerException
import core.grammar.FailureG
import core.transformation._
import transformations.bytecode.SimpleByteCode
import transformations.javac.types.TypeC

import scala.collection.mutable

case class MissingToInstructionsFor(clazz: Any) extends CompilerException {
  override def toString = s"missing transformation for ${clazz.getClass.getSimpleName}"
}

object ExpressionC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(SimpleByteCode, TypeC)

  def getType(state: TransformationState): MetaObject => Any = expression => getGetTypeRegistry(state)(expression.clazz)(expression)

  def getGetTypeRegistry(state: TransformationState) = getState(state).getTypeRegistry

  def getToInstructions(state: TransformationState): MetaObject => Seq[MetaObject] = {
    expression => {
      val implementation = getExpressionToLines(state).getOrElse(expression.clazz, throw new MissingToInstructionsFor(expression.clazz))
      implementation(expression)
    }
  }

  def getExpressionToLines(state: TransformationState): ToInstructionsRegistry = getState(state).transformations

  private def getState(state: TransformationState): State = {
    state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  }

  override def transformGrammars(grammars: GrammarCatalogue) {
    grammars.create(ExpressionGrammar, FailureG)
  }

  class ToInstructionsRegistry extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  class GetTypeRegistry extends mutable.HashMap[AnyRef, MetaObject => Any]

  class State {
    val transformations = new ToInstructionsRegistry()
    val getTypeRegistry = new GetTypeRegistry
  }

  object ExpressionGrammar

}
