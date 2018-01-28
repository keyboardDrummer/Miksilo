package deltas.javac.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.NodePath
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import deltas.bytecode.types.TypeSkeleton

object ExpressionSkeleton extends DeltaWithGrammar with WithLanguageRegistry {

  override def dependencies: Set[Contract] = Set(TypeSkeleton)

  implicit class Expression(val node: Node) extends NodeWrapper[Node]

  def getType(compilation: Compilation): NodePath => Node = expression => {
    ???
    //getRegistry(compilation).instances(expression.shape).getType(builder, expression, parentScope)
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Type = {
    getInstance(compilation)(expression).getType(compilation, builder, expression, parentScope)
  }

  def getInstance(language: Language): NodeLike => ExpressionInstance = {
    expression => getRegistry(language).instances(expression.shape)
  }

  def getToInstructions(compilation: Compilation): NodePath => Seq[Node] = {
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
