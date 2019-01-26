package deltas.javac.expressions.literals

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.RegexGrammar
import core.deltas.{Contract, DeltaWithGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.coreInstructions.longs.PushLongDelta
import deltas.bytecode.types.LongTypeDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.expressions.ConvertsToByteCodeDelta

object LongLiteralDelta extends DeltaWithGrammar with ExpressionInstance with ConvertsToByteCodeDelta {
  val shape = LongLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionDelta, SmallIntegerConstantDelta)

  private def parseLong(number: String) = java.lang.Long.parseLong(number.dropRight(1))

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val longGrammar : BiGrammar = RegexGrammar("""-?\d+l""".r).map[String, Long](
      number => parseLong(number), l => s"${l}l") as ValueKey asNode LongLiteralKey
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(longGrammar)
  }

  def literal(value: Long) = new Node(LongLiteralKey, ValueKey -> value)

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(PushLongDelta.constant(getValue(literal).toInt))
  }

  def getValue(literal: Node): Long = literal(ValueKey).asInstanceOf[Long]

  object LongLiteralKey extends NodeShape

  object ValueKey extends NodeField

  override def description: String = "Adds the usage of long literals by putting an l after the number."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, LongTypeDelta.constraintType)
  }
}
