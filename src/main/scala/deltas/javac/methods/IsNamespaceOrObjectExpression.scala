package deltas.javac.methods

import core.deltas.path.NodePath
import core.deltas.{Delta, HasShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope

trait IsNamespaceOrObjectExpression extends Delta with HasShape {
  def getScopeDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Declaration

  override def inject(language: Language): Unit = {
    super.inject(language)
    NamespaceOrObjectExpression.namespaceOrObjectExpression.add(language, this)
  }
}
