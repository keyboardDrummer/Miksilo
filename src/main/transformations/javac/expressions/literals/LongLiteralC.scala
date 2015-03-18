package transformations.javac.expressions.literals

import core.biGrammar.BiGrammar
import core.grammar.RegexG
import core.particles.grammars.GrammarCatalogue
import core.particles.path.Path
import core.particles.{CompilationState, Contract, MetaObject}
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.bytecode.coreInstructions.longs.LongConstantC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.types.LongTypeC

object LongLiteralC extends ExpressionInstance {
  val key = LongLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, IntegerConstantC)

  def parseLong(number: String) = java.lang.Long.parseLong(number.dropRight(1))

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val longGrammar : BiGrammar = (new RegexG("""-?\d+l""".r) : BiGrammar) ^^
      (number => parseLong(number.asInstanceOf[String]), l => Some(s"${l}l")) ^^ parseMap(LongLiteralKey, ValueKey)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(longGrammar)
  }

  def literal(value: Long) = new MetaObject(LongLiteralKey, ValueKey -> value)

  override def toByteCode(literal: Path, state: CompilationState): Seq[MetaObject] = {
    Seq(LongConstantC.constant(getValue(literal).toInt))
  }

  def getValue(literal: MetaObject) = literal(ValueKey).asInstanceOf[Long]

  override def getType(expression: Path, state: CompilationState): MetaObject = LongTypeC.longType

  object LongLiteralKey

  object ValueKey

  override def description: String = "Adds the usage of long literals by putting an l after the number."
}
