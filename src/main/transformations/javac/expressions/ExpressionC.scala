package transformations.javac.expressions

import core.exceptions.CompilerException
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.types.TypeC

import scala.collection.mutable
import scala.util.Try

case class GetTypeNotFound(key: Any) extends CompilerException {
  override def toString = s"'get type' method not found for class ${MetaObject.classDebugRepresentation(key)}"
}

object ExpressionC extends GrammarTransformation {

  case class MissingToInstructionsFor(clazz: Any) extends CompilerException {
    override def toString = s"missing transformation for ${clazz.getClass.getSimpleName}"
  }

  override def dependencies: Set[Contract] = Set(TypeC)

  def getType(state: TransformationState): MetaObject => MetaObject = expression => {
    val getTypeMethod = Try(getGetTypeRegistry(state)(expression.clazz)).
      recover({case e: NoSuchElementException => throw new GetTypeNotFound(expression.clazz)}).get
    getTypeMethod(expression)
  }

  def getGetTypeRegistry(state: TransformationState) = getState(state).getTypeRegistry

  private def getState(state: TransformationState): State = {
    state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  }

  def getToInstructions(state: TransformationState): MetaObject => Seq[MetaObject] = {
    expression => {
      val implementation = getExpressionToLines(state).getOrElse(expression.clazz, throw new MissingToInstructionsFor(expression.clazz))
      implementation(expression)
    }
  }

  def getExpressionToLines(state: TransformationState): ToInstructionsRegistry = getState(state).transformations

  override def transformGrammars(grammars: GrammarCatalogue) {
    val core = grammars.create(CoreGrammar)
    grammars.create(ExpressionGrammar, core)
  }

  class ToInstructionsRegistry extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  class GetTypeRegistry extends mutable.HashMap[AnyRef, MetaObject => MetaObject]

  class State {
    val transformations = new ToInstructionsRegistry()
    val getTypeRegistry = new GetTypeRegistry
  }

  object CoreGrammar
  object ExpressionGrammar

}
