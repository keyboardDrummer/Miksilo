package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeLike, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.javac.types.BooleanTypeDelta

object TernaryDelta extends DeltaWithGrammar with ExpressionInstance {
  def falseBranch[T <: NodeLike](metaObject: T) = metaObject(FalseBranch).asInstanceOf[T]

  def trueBranch[T <: NodeLike](metaObject: T) = metaObject(TrueBranch).asInstanceOf[T]

  def getCondition[T <: NodeLike](metaObject: T) = {
    metaObject(Condition).asInstanceOf[T]
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val parseTernary = (expressionGrammar.as(Condition) ~~< "?") ~~
      (expressionGrammar.as(TrueBranch) ~~< ":") ~~
      expressionGrammar.as(FalseBranch) asLabelledNode Shape
    expressionGrammar.addAlternative(parseTernary)
  }

  def ternary(condition: Node, trueBranch: Node, falseBranch: Node) = new Node(Shape,
    FalseBranch -> falseBranch,
    TrueBranch -> trueBranch,
    Condition -> condition)

  object FalseBranch extends NodeField

  object TrueBranch extends NodeField

  object Condition extends NodeField

  object Shape extends NodeShape

  override val shape = Shape

  override def description: String = "Adds the ternary operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, _ternary: NodePath, _type: Type, parentScope: Scope): Unit = {
    val condition = TernaryDelta.getCondition(_ternary)
    val truePath = TernaryDelta.trueBranch(_ternary)
    val falsePath = TernaryDelta.falseBranch(_ternary)
    val conditionType = ExpressionDelta.getType(compilation, builder, condition, parentScope)
    builder.typesAreEqual(BooleanTypeDelta.constraintType, conditionType)

    val trueType = ExpressionDelta.getType(compilation, builder, truePath, parentScope)
    val falseType = ExpressionDelta.getType(compilation, builder, falsePath, parentScope)

    builder.typesAreEqual(_type, builder.getCommonSuperType(trueType, falseType))
  }
}
