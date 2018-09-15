package deltas.javac.expressions

import core.deltas.ShapeProperty
import core.deltas.path.NodePath
import core.language.node.{Node, NodeLike, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

object ByteCodeExpressionSkeleton {

  implicit class Expression(val node: Node) extends NodeWrapper[Node]

  def getType(compilation: Compilation): NodePath => Node = expression => {
    getInstance(compilation)(expression).getType(expression, compilation)
  }

  def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    getInstance(compilation)(expression).constraints(compilation, builder, expression, _type, parentScope)
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Type = {
    getInstance(compilation)(expression).getType(compilation, builder, expression, parentScope)
  }

  def getInstance(language: Language): NodeLike => ExpressionInstance = {
    expression => expressionInstances.get(language, expression.shape)
  }

  val expressionInstances = new ShapeProperty[ExpressionInstance]
}
