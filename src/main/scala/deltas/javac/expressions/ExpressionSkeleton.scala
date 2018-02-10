package deltas.javac.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.TypeSkeleton

object ExpressionSkeleton extends DeltaWithGrammar with WithLanguageRegistry {

  override def dependencies: Set[Contract] = Set(TypeSkeleton)

  implicit class Expression(val node: Node) extends NodeWrapper[Node]

  def getType(compilation: Compilation): Path => Node = expression => {
    getRegistry(compilation).instances(expression.shape).getType(expression, compilation)
  }

  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: Path, _type: Type, parentScope: Scope): Unit = {
    getInstance(compilation)(expression).constraints(compilation, builder, expression, _type, parentScope)
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: Path, parentScope: Scope): Type = {
    getInstance(compilation)(expression).getType(compilation, builder, expression, parentScope)
  }

  def getInstance(language: Language): NodeLike => ExpressionInstance = {
    expression => getRegistry(language).instances(expression.shape)
  }

  def getToInstructions(compilation: Compilation): Path => Seq[Node] = {
    val getInstance2 = getInstance(compilation)
    expression => getInstance2(expression).toByteCode(expression, compilation)
  }

  def getToInstructionsRegistry(state: Language) = getRegistry(state).instances

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val core = grammars.create(CoreGrammar)
    grammars.create(ExpressionGrammar, core)
  }

  def createRegistry = new Registry()
  class Registry {
    val instances = new ShapeRegistry[ExpressionInstance]
  }

  object CoreGrammar extends GrammarKey
  object ExpressionGrammar extends GrammarKey

  override def description: String = "Introduces the concept of an expression."
}
