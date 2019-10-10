package deltas.solidity

import core.deltas.path.NodePath
import core.deltas.{Contract, Delta}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.{ExpressionDelta, IntLiteralDelta, IsExpression}

object SolidityIntLiteralDelta extends Delta {

  override def inject(language: Language): Unit = {
    ExpressionDelta.expressionInstances.add(language, IntLiteralDelta.Shape, new IsExpression {
      override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {

      }
    })
    super.inject(language)
  }

  override def dependencies: Set[Contract] = Set(IntLiteralDelta)

  override def description = "Overrides the type checking of the int literal"
}
