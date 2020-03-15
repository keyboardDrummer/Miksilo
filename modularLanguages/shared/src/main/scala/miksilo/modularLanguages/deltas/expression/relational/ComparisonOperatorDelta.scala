package miksilo.modularLanguages.deltas.expression.relational

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance, LeftAssociativeBinaryOperatorDelta}
import miksilo.modularLanguages.deltas.javac.types.BooleanTypeDelta

trait ComparisonOperatorDelta extends LeftAssociativeBinaryOperatorDelta with ExpressionInstance {
  import LeftAssociativeBinaryOperatorDelta._

  val shape: NodeShape

  override def precedenceGrammarKey = RelationalPrecedenceDelta.Grammar

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val firstType = ExpressionDelta.getType(compilation, builder, expression.left, parentScope)
    val secondType = ExpressionDelta.getType(compilation, builder, expression.right, parentScope)
    builder.typesAreEqual(firstType, secondType)
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }

  def keyword: String

  override def dependencies: Set[Contract] = Set(RelationalPrecedenceDelta)
}
