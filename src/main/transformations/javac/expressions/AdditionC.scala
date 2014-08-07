package transformations.javac.expressions

import core.grammar.{Grammar, seqr}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.AddIntegersC
import transformations.javac.types.{IntTypeC, TypeC}

object AdditionC extends GrammarTransformation with ExpressionInstance {

  override def toByteCode(addition: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val toInstructions = ExpressionC.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(addition))
    val secondInstructions = toInstructions(getSecond(addition))
    firstInstructions ++ secondInstructions ++ Seq(AddIntegersC.addInteger)
  }

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = {
    val getType = ExpressionC.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeC.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeC.checkAssignableTo(state)(IntTypeC.intType, secondType)
    IntTypeC.intType
  }

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, AddIntegersC)

  override def transformGrammars(grammars: GrammarCatalogue) {
    val additiveGrammar = grammars.find(AddAdditivePrecedence.AdditiveExpressionGrammar)
    val parseAddition: Grammar = (additiveGrammar <~ "+") ~ additiveGrammar ^^ { case left seqr right => addition(left, right)}
    additiveGrammar.inner = additiveGrammar.inner | parseAddition
  }

  private def addition(first: Any, second: Any): MetaObject = addition(first.asInstanceOf[MetaObject], second.asInstanceOf[MetaObject])

  def addition(first: MetaObject, second: MetaObject) = new MetaObject(AdditionClazz) {
    data.put(FirstKey, first)
    data.put(SecondKey, second)
  }

  def getFirst(addition: MetaObject) = addition(FirstKey).asInstanceOf[MetaObject]

  def getSecond(addition: MetaObject) = addition(SecondKey).asInstanceOf[MetaObject]

  object AdditionClazz

  object FirstKey

  object SecondKey

  val key = AdditionClazz

}
