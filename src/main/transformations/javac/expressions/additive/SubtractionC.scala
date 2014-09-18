package transformations.javac.expressions.additive

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.SubtractIntegerC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}

object SubtractionC extends ExpressionInstance {
  object SubtractionKey
  object FirstKey
  object SecondKey

  def getFirst(subtraction: MetaObject) = subtraction(FirstKey).asInstanceOf[MetaObject]

  def getSecond(subtraction: MetaObject) = subtraction(SecondKey).asInstanceOf[MetaObject]

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, SubtractIntegerC)

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseSubtraction = (additiveGrammar <~~ "-") ~~  additiveGrammar ^^ parseMap(SubtractionKey, FirstKey, SecondKey)
    additiveGrammar.addOption(parseSubtraction)
  }

  def subtraction(first: Any, second: Any): MetaObject = subtraction(first.asInstanceOf[MetaObject], second.asInstanceOf[MetaObject])

  def subtraction(first: MetaObject, second: MetaObject) = new MetaObject(SubtractionKey) {
    data.put(FirstKey, first)
    data.put(SecondKey, second)
  }

  override val key: AnyRef = SubtractionKey

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = ???

  override def toByteCode(subtraction: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val toInstructions = ExpressionC.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(subtraction))
    val secondInstructions = toInstructions(getSecond(subtraction))
    firstInstructions ++ secondInstructions ++ Seq(SubtractIntegerC.subtractInteger)
  }
}
