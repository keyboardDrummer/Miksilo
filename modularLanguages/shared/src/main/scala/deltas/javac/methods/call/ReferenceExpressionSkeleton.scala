package deltas.javac.methods.call

import core.deltas.{Delta, HasShape, ShapeProperty}
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.TypedNodeField
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope

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
