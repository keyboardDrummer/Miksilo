package transformations.javac.expressions.literals

import core.grammar.RegexG
import core.grammarDocument.GrammarDocument
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.bytecode.coreInstructions.longs.LongConstantC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.LongTypeC

object LongLiteralC extends ExpressionInstance {
  val key = LongLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionC, IntegerConstantC)

  def parseLong(number: String) = java.lang.Long.parseLong(number.dropRight(1))

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val longGrammar : GrammarDocument = (new RegexG("""-?\d+l""".r) : GrammarDocument) ^^
      (number => parseLong(number.asInstanceOf[String]), l => Some(s"${l}l")) ^^ parseMap(LongLiteralKey, ValueKey)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.orToInner(longGrammar)
  }

  def literal(value: Long) = new MetaObject(LongLiteralKey, ValueKey -> value)

  override def toByteCode(literal: MetaObject, state: TransformationState): Seq[MetaObject] = {
    Seq(LongConstantC.constant(getValue(literal).toInt))
  }

  def getValue(literal: MetaObject) = literal(ValueKey).asInstanceOf[Long]

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = LongTypeC.longType

  object LongLiteralKey

  object ValueKey
}
