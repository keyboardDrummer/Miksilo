package transformations.javac.expressions

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.IntegerConstantC
import transformations.types.IntTypeC

object NumberLiteralC extends ExpressionInstance {
  val key = NumberLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionC, IntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseNumber = number ^^ (number => NumberLiteralC.literal(Integer.parseInt(number.asInstanceOf[String])))
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Int) = new MetaObject(NumberLiteralKey, ValueKey -> value)

  override def toByteCode(literal: MetaObject, state: TransformationState): Seq[MetaObject] = {
    Seq(IntegerConstantC.integerConstant(getValue(literal)))
  }

  def getValue(literal: MetaObject) = literal(ValueKey).asInstanceOf[Int]

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = IntTypeC.intType

  object NumberLiteralKey

  object ValueKey

}
