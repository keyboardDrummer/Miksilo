package transformations.javac.expressions

import core.exceptions.CompilerException
import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.types.TypeSkeleton

import scala.util.Try

case class GetTypeNotFound(key: Any) extends CompilerException {
  override def toString = s"'get type' method not found for class ${MetaObject.classDebugRepresentation(key)}"
}

object ExpressionSkeleton extends ParticleWithGrammar with WithState {

  case class MissingToInstructionsFor(clazz: Any) extends CompilerException {
    override def toString = s"missing transformation for ${clazz.getClass.getSimpleName}"
  }

  override def dependencies: Set[Contract] = Set(TypeSkeleton)

  def getType(state: CompilationState): MetaObject => MetaObject = expression => {
    val getTypeMethod = Try(getGetTypeRegistry(state)(expression.clazz)).
      recover({case e: NoSuchElementException => throw new GetTypeNotFound(expression.clazz)}).get
    getTypeMethod(expression)
  }

  def getGetTypeRegistry(state: CompilationState) = getState(state).getTypeRegistry

  def getToInstructions(state: CompilationState): MetaObject => Seq[MetaObject] = {
    expression => {
      val implementation = getExpressionToLines(state).getOrElse(expression.clazz, throw new MissingToInstructionsFor(expression.clazz))
      implementation(expression)
    }
  }

  def getExpressionToLines(state: CompilationState) = getState(state).expressionToInstructions

  override def transformGrammars(grammars: GrammarCatalogue) {
    val core = grammars.create(CoreGrammar)
    grammars.create(ExpressionGrammar, core)
  }

  def createState = new State()
  class State {
    val expressionToInstructions = new ClassRegistry[MetaObject => Seq[MetaObject]]
    val getTypeRegistry = new ClassRegistry[MetaObject => MetaObject]
  }

  object CoreGrammar
  object ExpressionGrammar

  override def description: String = "Introduces the concept of an expression."
}
