package transformations.javac.expressions.literals

import core.grammar.RegexG
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.{IntegerConstantC, LongConstantC}
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.LongTypeC

object LongLiteralC extends ExpressionInstance {
  val key = LongLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionC, IntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseLong = new RegexG("""-?\d+l""".r) ^^ (number => literal(java.lang.Long.parseLong(number.asInstanceOf[String].dropRight(1))))
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseLong
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
