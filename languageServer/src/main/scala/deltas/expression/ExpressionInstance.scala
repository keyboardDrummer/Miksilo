package deltas.expression

import core.deltas.path.NodePath
import core.deltas.{Contract, Delta, HasShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

trait IsExpression {
  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit
}

trait ExpressionInstance extends Delta with HasShape with IsExpression  {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ExpressionDelta.expressionInstances.add(language, this)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)
}