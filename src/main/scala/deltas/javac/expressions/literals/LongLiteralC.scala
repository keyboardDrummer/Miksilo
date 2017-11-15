package deltas.javac.expressions.literals

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.RegexG
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.Path
import core.particles.{Compilation, Contract, Language}
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.coreInstructions.longs.PushLongDelta
import deltas.bytecode.types.LongTypeC
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object LongLiteralC extends ExpressionInstance {
  val key = LongLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantDelta)

  def parseLong(number: String) = java.lang.Long.parseLong(number.dropRight(1))

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val longGrammar : BiGrammar = RegexG("""-?\d+l""".r) ^^
      (number => parseLong(number.asInstanceOf[String]), l => Some(s"${l}l")) as ValueKey asNode LongLiteralKey
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(longGrammar)
  }

  def literal(value: Long) = new Node(LongLiteralKey, ValueKey -> value)

  override def toByteCode(literal: Path, compilation: Compilation): Seq[Node] = {
    Seq(PushLongDelta.constant(getValue(literal).toInt))
  }

  def getValue(literal: Node) = literal(ValueKey).asInstanceOf[Long]

  override def getType(expression: Path, compilation: Compilation): Node = LongTypeC.longType

  object LongLiteralKey extends NodeClass

  object ValueKey extends NodeField

  override def description: String = "Adds the usage of long literals by putting an l after the number."
}
