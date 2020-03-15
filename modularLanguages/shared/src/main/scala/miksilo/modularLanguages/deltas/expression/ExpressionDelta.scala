package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.bigrammar.grammars.BiFallback
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar, ShapeProperty}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{ConcreteType, Type}
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton

object ExpressionDelta extends DeltaWithGrammar with ExpressionInstance {

  val value = DefaultShape.create()

  object DefaultShape extends NodeShape {
    override def toString = "hole"
  }

  override def shape: NodeShape = DefaultShape

  implicit class Expression(val node: Node) extends NodeWrapper[Node]

  val constraintType = new TypedNodeField[Type]("constraintType")
  val nodeType = new TypedNodeField[Node]("nodeType")

  def addConstraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    constraintType(expression) = _type
    getInstance(compilation)(expression).constraints(compilation, builder, expression, _type, parentScope)
  }

  def cachedNodeType(compilation: Compilation, expression: NodePath): Node = {
    nodeType.get(expression) match {
      case None =>
        val result = TypeSkeleton.fromConstraintType(getCachedType(compilation, expression))
        nodeType(expression) = result
        result
      case Some(result) => result
    }
  }

  def getCachedType(compilation: Compilation, expression: NodePath): ConcreteType = {
    compilation.proofs.resolveType(constraintType(expression)).asInstanceOf[ConcreteType]
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Type = {
    val result = builder.typeVariable(Some(expression))
    addConstraints(compilation, builder, expression, result, parentScope)
    result
  }

  def getInstance(language: Language): NodeLike => IsExpression = {
    expression => expressionInstances(language, expression.shape)
  }

  val expressionInstances = new ShapeProperty[IsExpression]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val core = grammars.create(LastPrecedenceGrammar, new BiFallback(value, "value"))
    grammars.create(FirstPrecedenceGrammar, core)
  }

  object LastPrecedenceGrammar extends GrammarKey
  object FirstPrecedenceGrammar extends GrammarKey

  override def description: String = "Introduces the concept of an expression."

  override def dependencies: Set[Contract] = Set.empty

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {

  }
}
