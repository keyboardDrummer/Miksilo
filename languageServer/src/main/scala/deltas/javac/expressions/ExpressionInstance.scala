package deltas.javac.expressions

import core.deltas._
import core.deltas.path.NodePath
import core.language.node.Node
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.ExpressionDelta

trait ExpressionInstance extends Delta with HasShape {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ExpressionDelta.expressionInstances.add(language, this)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Type = {
    val result = builder.typeVariable()
    constraints(compilation, builder, expression, result, parentScope)
    result
  }

  def getType(expression: NodePath, compilation: Compilation): Node //TODO remove
}
