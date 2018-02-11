package deltas.javac.expressions.literals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeShape}
import core.deltas.path.NodePath
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.types.BooleanTypeDelta

object BooleanLiteralDelta extends ExpressionInstance {
  val key = LiteralBooleanKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val parseNumber = "true" ~> value(literal(true)) | "false" ~> value(literal(false))
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Boolean) = new Node(LiteralBooleanKey, ValueKey -> value)

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(SmallIntegerConstantDelta.integerConstant(if (getValue(literal)) 1 else 0))
  }

  def getValue(literal: Node) = literal(ValueKey).asInstanceOf[Boolean]

  override def getType(expression: NodePath, compilation: Compilation): Node = BooleanTypeDelta.booleanType

  object LiteralBooleanKey extends NodeShape

  object ValueKey extends NodeField

  override def description: String = "Adds the boolean literals 'true' and 'false'"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }
}
