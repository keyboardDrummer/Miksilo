package deltas.javac.expressions

import core.deltas._
import core.deltas.path.NodePath
import core.language.node.Node
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

trait ExpressionInstance extends DeltaWithGrammar with HasShape {

  override def inject(language: Language): Unit = {
    ExpressionSkeleton.getRegistry(language).instances.put(shape, this)
    super.inject(language)
  }

  def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node]

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = ???

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Type = {
    val result = builder.typeVariable()
    constraints(compilation, builder, expression, result, parentScope)
    result
  }

  def getType(expression: NodePath, compilation: Compilation): Node //TODO remove
}
