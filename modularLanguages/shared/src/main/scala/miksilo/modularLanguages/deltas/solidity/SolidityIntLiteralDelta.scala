package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, Delta}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, IntLiteralDelta, IsExpression}

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
