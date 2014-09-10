package transformations.javac.expressions.additive

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.integers.SubtractIntegerC
import transformations.javac.expressions.ExpressionC

object SubtractionC extends GrammarTransformation {
  object SubtractionKey
  object FirstKey
  object SecondKey

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(SubtractionKey, subtraction => {
      val toInstructions = ExpressionC.getToInstructions(state)
      val firstInstructions = toInstructions(getFirst(subtraction))
      val secondInstructions = toInstructions(getSecond(subtraction))
      firstInstructions ++ secondInstructions ++ Seq(SubtractIntegerC.subtractInteger)
    })
  }

  def getFirst(subtraction: MetaObject) = subtraction(FirstKey).asInstanceOf[MetaObject]

  def getSecond(subtraction: MetaObject) = subtraction(SecondKey).asInstanceOf[MetaObject]

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, SubtractIntegerC)

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseSubtraction = (additiveGrammar <~ "-") ~ additiveGrammar ^^ parseMap(SubtractionKey, FirstKey, SecondKey)
    additiveGrammar.addOption(parseSubtraction)
  }

  def subtraction(first: Any, second: Any): MetaObject = subtraction(first.asInstanceOf[MetaObject], second.asInstanceOf[MetaObject])

  def subtraction(first: MetaObject, second: MetaObject) = new MetaObject(SubtractionKey) {
    data.put(FirstKey, first)
    data.put(SecondKey, second)
  }
}
