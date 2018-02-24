package deltas.javac.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.TypeSkeleton

object ExpressionSkeleton extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(TypeSkeleton)

  implicit class Expression(val node: Node) extends NodeWrapper[Node]

  def getType(compilation: Compilation): NodePath => Node = expression => {
    getInstance(compilation)(expression).getType(expression, compilation)
  }

  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    getInstance(compilation)(expression).constraints(compilation, builder, expression, _type, parentScope)
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Type = {
    getInstance(compilation)(expression).getType(compilation, builder, expression, parentScope)
  }

  def getInstance(language: Language): NodeLike => ExpressionInstance = {
    expression => expressionInstances.get(language, expression.shape)
  }

  def getToInstructions(compilation: Compilation): NodePath => Seq[Node] = {
    expression => getInstance(compilation)(expression).toByteCode(expression, compilation)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val core = grammars.create(CoreGrammar)
    grammars.create(ExpressionGrammar, core)
  }

  val expressionInstances = new ShapeProperty[ExpressionInstance]

  object CoreGrammar extends GrammarKey
  object ExpressionGrammar extends GrammarKey

  override def description: String = "Introduces the concept of an expression."
}
