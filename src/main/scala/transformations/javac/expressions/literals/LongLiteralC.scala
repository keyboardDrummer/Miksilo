package transformations.javac.expressions.literals

import core.bigrammar.BiGrammar
import core.grammar.RegexG
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import core.particles.{CompilationState, Contract}
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.coreInstructions.longs.PushLongC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.bytecode.types.LongTypeC

object LongLiteralC extends ExpressionInstance {
  val key = LongLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantC)

  def parseLong(number: String) = java.lang.Long.parseLong(number.dropRight(1))

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val longGrammar : BiGrammar = (new RegexG("""-?\d+l""".r) : BiGrammar) ^^
      (number => parseLong(number.asInstanceOf[String]), l => Some(s"${l}l")) ^^ parseMap(LongLiteralKey, ValueKey)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(longGrammar)
  }

  def literal(value: Long) = new Node(LongLiteralKey, ValueKey -> value)

  override def toByteCode(literal: Path, state: CompilationState): Seq[Node] = {
    Seq(PushLongC.constant(getValue(literal).toInt))
  }

  def getValue(literal: Node) = literal(ValueKey).asInstanceOf[Long]

  override def getType(expression: Path, state: CompilationState): Node = LongTypeC.longType

  object LongLiteralKey

  object ValueKey

  override def description: String = "Adds the usage of long literals by putting an l after the number."
}
