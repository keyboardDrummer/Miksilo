package transformations.javac.expressions.equality

import core.grammar._
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.longs.CompareLongC
import transformations.bytecode.extraBooleanInstructions.NotEqualInstructionC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.BooleanTypeC

object EqualityC extends ExpressionInstance {
  override def dependencies: Set[Contract] = Set(AddEqualityPrecedence)

  def getFirst(equality: MetaObject) = equality(FirstKey).asInstanceOf[MetaObject]

  def getSecond(equality: MetaObject) = equality(SecondKey).asInstanceOf[MetaObject]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val equalityGrammar = grammars.find(AddEqualityPrecedence.EqualityExpressionGrammar)
    val parseEquality = (equalityGrammar <~ "==") ~ equalityGrammar ^^ { case left ~ right => equality(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject])}
    equalityGrammar.orToInner(parseEquality)
  }

  def equality(first: MetaObject, second: MetaObject) = new MetaObject(EqualityKey, FirstKey -> first, SecondKey -> second)

  object EqualityKey

  object FirstKey

  object SecondKey

  override val key: AnyRef = EqualityKey

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = BooleanTypeC.booleanType

  override def toByteCode(equality: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val first = getFirst(equality)
    val second = getSecond(equality)
    val toInstructions = ExpressionC.getToInstructions(state)
    toInstructions(first) ++ toInstructions(second) ++ Seq(CompareLongC.compareLong, NotEqualInstructionC.notEqual)
  }
}
