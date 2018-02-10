package deltas.javac.expressions

import core.deltas._
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.Path
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

trait ExpressionInstance extends DeltaWithGrammar {
  val key: NodeShape

  override def inject(language: Language): Unit = {
    ExpressionSkeleton.getRegistry(language).instances.put(key, this)
    super.inject(language)
  }

  def toByteCode(expression: Path, compilation: Compilation): Seq[Node]

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: Path, _type: Type, parentScope: Scope): Unit = ???

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: Path, parentScope: Scope): Type = {
    val result = builder.typeVariable()
    constraints(compilation, builder, expression, result, parentScope)
    result
  }

  def getType(expression: Path, compilation: Compilation): Node //TODO remove
}
