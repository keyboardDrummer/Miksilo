package miksilo.modularLanguages.deltas.javac.methods.call

import miksilo.modularLanguages.core.deltas.{Delta, HasShape, ShapeProperty}
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.TypedNodeField
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.objects.Reference
import miksilo.languageServer.core.smarts.scopes.objects.Scope

object ReferenceExpressionSkeleton {
  val instances = new ShapeProperty[ReferenceExpression]
  val references = new TypedNodeField[Reference]("definedReference")

  def getReference(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Reference = {
    val result = instances(compilation, expression.shape).getReference(compilation, builder, expression, parentScope)
    references(expression) = result
    result
  }
}


trait ReferenceExpression {
  def getReference(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Reference
}

trait ReferenceExpressionDelta extends Delta with HasShape with ReferenceExpression {
  override def inject(language: Language): Unit = {
    super.inject(language)
    ReferenceExpressionSkeleton.instances.add(language, shape, this)
  }
}
