package transformations.javac.expressions.literals

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.IntTypeC

object IntLiteralC extends ExpressionInstance {
  val key = IntLiteralKey

  override def dependencies: Set[Contract] = Set(ExpressionC, IntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseNumber = number ^^ (number => Integer.parseInt(number.asInstanceOf[String]), i => Some(i)) ^^ parseMap(IntLiteralKey, ValueKey)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Int) = new MetaObject(IntLiteralKey, ValueKey -> value)

  override def toByteCode(literal: MetaObject, state: TransformationState): Seq[MetaObject] = {
    Seq(IntegerConstantC.integerConstant(getValue(literal)))
  }

  def getValue(literal: MetaObject) = literal(ValueKey).asInstanceOf[Int]

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = IntTypeC.intType

  object IntLiteralKey

  object ValueKey

}
