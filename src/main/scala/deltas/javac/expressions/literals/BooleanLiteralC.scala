package deltas.javac.expressions.literals

import core.particles._
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.Path
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.types.BooleanTypeC

object BooleanLiteralC extends ExpressionInstance {
  val key = LiteralBooleanKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val parseNumber = "true" ~> value(literal(true)) | "false" ~> value(literal(false))
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Boolean) = new Node(LiteralBooleanKey, ValueKey -> value)

  override def toByteCode(literal: Path, compilation: Compilation): Seq[Node] = {
    Seq(SmallIntegerConstantDelta.integerConstant(if (getValue(literal)) 1 else 0))
  }

  def getValue(literal: Node) = literal(ValueKey).asInstanceOf[Boolean]

  override def getType(expression: Path, compilation: Compilation): Node = BooleanTypeC.booleanType

  object LiteralBooleanKey extends NodeClass

  object ValueKey extends NodeField

  override def description: String = "Adds the boolean literals 'true' and 'false'"
}
