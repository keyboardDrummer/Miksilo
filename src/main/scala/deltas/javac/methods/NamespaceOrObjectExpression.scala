package deltas.javac.methods

import core.deltas.ShapeProperty
import core.deltas.path.NodePath
import core.language.Compilation
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.javac.expressions.ExpressionSkeleton

object NamespaceOrObjectExpression {

  def getScope(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Scope = {
    val scopeDeclaration = getScopeDeclaration(compilation, builder, expression, scope)
    val targetScope = builder.scopeVariable()
    builder.add(ResolveNamespaceOrObjectExpressionToScope(scopeDeclaration, targetScope))
    targetScope
  }

  def getScopeDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Declaration = {
    namespaceOrObjectExpression.get(compilation).get(expression.shape).fold({
      val _type = ExpressionSkeleton.getType(compilation, builder, expression, scope)
      builder.getDeclarationOfType(_type)
    })(reference => reference.getScopeDeclaration(compilation, builder, expression, scope))
  }

  val namespaceOrObjectExpression: ShapeProperty[IsNamespaceOrObjectExpression] = new ShapeProperty[IsNamespaceOrObjectExpression]
}
