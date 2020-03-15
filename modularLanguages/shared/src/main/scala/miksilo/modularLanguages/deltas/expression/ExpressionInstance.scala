package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, Delta, HasShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type

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