package transformations.javac.expressions.literals

import core.grammar.Delimiter
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.{LongConstantC, IntegerConstantC}
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.LongTypeC

object LongLiteralC extends ExpressionInstance {
  val key = LongLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionC, IntegerConstantC)

  //TODO fix the incorrect parsing.
  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseLong = (number <~ new Delimiter("l")) ^^ (number => literal(java.lang.Long.parseLong(number.asInstanceOf[String])))
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseLong
  }

  def literal(value: Long) = new MetaObject(LongLiteralKey, ValueKey -> value)

  override def toByteCode(literal: MetaObject, state: TransformationState): Seq[MetaObject] = {
    Seq(LongConstantC.constant(getValue(literal)))
  }

  def getValue(literal: MetaObject) = literal(ValueKey).asInstanceOf[Long]

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = LongTypeC.longType

  object LongLiteralKey

  object ValueKey
}
