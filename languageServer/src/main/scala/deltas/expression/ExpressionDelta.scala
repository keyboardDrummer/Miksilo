package deltas.expression

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar, ShapeProperty}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{ConcreteType, Type}
import deltas.bytecode.types.TypeSkeleton

object ExpressionDelta extends DeltaWithGrammar {

  implicit class Expression(val node: Node) extends NodeWrapper[Node]

  val constraintType = new TypedNodeField[Type]("constraintType")
  val nodeType = new TypedNodeField[Node]("nodeType")
  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    constraintType(expression) = _type
    getInstance(compilation)(expression).constraints(compilation, builder, expression, _type, parentScope)
  }

  def cachedNodeType(compilation: Compilation, expression: NodePath): Node = {
    nodeType.get(expression) match {
      case None =>
        val result = TypeSkeleton.fromConstraintType(compilation, getCachedType(compilation, expression))
        nodeType(expression) = result
        result
      case Some(result) => result
    }
  }

  def getCachedType(compilation: Compilation, expression: NodePath): ConcreteType = {
    compilation.proofs.resolveType(constraintType(expression)).asInstanceOf[ConcreteType]
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Type = {
    val result = getInstance(compilation)(expression).getType(compilation, builder, expression, parentScope)
    constraintType(expression) = result
    result
  }

  def getInstance(language: Language): NodeLike => ExpressionInstance = {
    expression => expressionInstances(language, expression.shape)
  }

  val expressionInstances = new ShapeProperty[ExpressionInstance]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val core = grammars.create(LastPrecedenceGrammar)
    grammars.create(FirstPrecedenceGrammar, core)
  }

  object LastPrecedenceGrammar extends GrammarKey
  object FirstPrecedenceGrammar extends GrammarKey

  override def description: String = "Introduces the concept of an expression."

  override def dependencies: Set[Contract] = Set.empty
}
