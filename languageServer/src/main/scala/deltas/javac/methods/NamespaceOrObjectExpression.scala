package deltas.javac.methods

import core.deltas.ShapeProperty
import core.deltas.path.NodePath
import core.language.Compilation
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.expression.ExpressionDelta

object NamespaceOrObjectExpression {

  def getScope(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Scope = {
    builder.getDeclaredScope(getScopeDeclaration(compilation, builder, expression, scope))
  }

  def getScopeDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Declaration = {
    namespaceOrObjectExpression.get(compilation).get(expression.shape).fold({
      val _type = ExpressionDelta.getType(compilation, builder, expression, scope)
      builder.getDeclarationOfType(_type)
    })(reference => reference.getScopeDeclaration(compilation, builder, expression, scope))
  }

  val namespaceOrObjectExpression: ShapeProperty[IsNamespaceOrObjectExpression] = new ShapeProperty[IsNamespaceOrObjectExpression]
}
