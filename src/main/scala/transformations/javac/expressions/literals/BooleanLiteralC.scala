package transformations.javac.expressions.literals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantDelta$
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.types.BooleanTypeC

object BooleanLiteralC extends ExpressionInstance {
  val key = LiteralBooleanKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantDelta$)

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val parseNumber = "true" ~> produce(literal(true)) | "false" ~> produce(literal(false))
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Boolean) = new Node(LiteralBooleanKey, ValueKey -> value)

  override def toByteCode(literal: Path, state: CompilationState): Seq[Node] = {
    Seq(SmallIntegerConstantDelta$.integerConstant(if (getValue(literal)) 1 else 0))
  }

  def getValue(literal: Node) = literal(ValueKey).asInstanceOf[Boolean]

  override def getType(expression: Path, state: CompilationState): Node = BooleanTypeC.booleanType

  object LiteralBooleanKey

  object ValueKey

  override def description: String = "Adds the boolean literals 'true' and 'false'"
}
