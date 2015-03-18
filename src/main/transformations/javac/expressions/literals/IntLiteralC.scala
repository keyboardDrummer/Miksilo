package transformations.javac.expressions.literals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.types.IntTypeC

object IntLiteralC extends ExpressionInstance {
  val key = IntLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, IntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseNumber = number ^^ (number => Integer.parseInt(number.asInstanceOf[String]), i => Some(i)) ^^ parseMap(IntLiteralKey, ValueKey)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Int) = new Node(IntLiteralKey, ValueKey -> value)

  override def toByteCode(literal: Path, state: CompilationState): Seq[Node] = {
    Seq(IntegerConstantC.integerConstant(getValue(literal)))
  }

  def getValue(literal: Node) = literal(ValueKey).asInstanceOf[Int]

  override def getType(expression: Path, state: CompilationState): Node = IntTypeC.intType

  object IntLiteralKey

  object ValueKey

  override def description: String = "Adds the usage of int literals."
}
