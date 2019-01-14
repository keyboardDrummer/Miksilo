package deltas.expression.logical

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.prefix.PrefixOperatorDelta
import deltas.expression.{ExpressionDelta, JavaExpressionInstance}
import deltas.javac.types.BooleanTypeDelta

object LogicalNotDelta extends PrefixOperatorDelta with JavaExpressionInstance {

  override def getType(expression: NodePath, compilation: Compilation): Node = BooleanTypeDelta.booleanType

  override def description: String = "Adds the ! (not) operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val targetType = ExpressionDelta.getType(compilation, builder, expression(Target).asInstanceOf[NodePath], parentScope)
    builder.typesAreEqual(targetType, BooleanTypeDelta.constraintType)
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }

  override def keyword = "!"
}
