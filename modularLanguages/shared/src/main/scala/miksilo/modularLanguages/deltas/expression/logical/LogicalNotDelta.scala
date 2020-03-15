package miksilo.modularLanguages.deltas.expression.logical

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.expression.prefix.PrefixOperatorDelta
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}
import miksilo.modularLanguages.deltas.javac.types.BooleanTypeDelta

object LogicalNotDelta extends PrefixOperatorDelta with ExpressionInstance {

  override def description: String = "Adds the ! (not) operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val targetType = ExpressionDelta.getType(compilation, builder, expression(Target).asInstanceOf[NodePath], parentScope)
    builder.typesAreEqual(targetType, BooleanTypeDelta.constraintType)
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }

  override def keyword = "!"
}
