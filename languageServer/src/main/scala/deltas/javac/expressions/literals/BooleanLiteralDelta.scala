package deltas.javac.expressions.literals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.types.BooleanTypeDelta

object BooleanLiteralDelta extends ExpressionInstance {
  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val parseNumber = ("true" ~> value(true) | "false" ~> value(false)).as(Value).asNode(Shape)
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Boolean) = new Node(Shape, Value -> value)

  def getValue(literal: Node) = literal(Value).asInstanceOf[Boolean]

  override def getType(expression: NodePath, compilation: Compilation): Node = BooleanTypeDelta.booleanType

  object Shape extends NodeShape

  object Value extends NodeField

  override def description: String = "Adds the boolean literals 'true' and 'false'"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }
}
