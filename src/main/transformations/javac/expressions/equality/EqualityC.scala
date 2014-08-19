package transformations.javac.expressions.equality

import core.grammar._
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.longs.CompareLongC
import transformations.bytecode.extraBooleanInstructions.{IntegerEqualsInstructionC, NotInstructionC}
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.{BooleanTypeC, IntTypeC, LongTypeC, TypeC}

object EqualityC extends ExpressionInstance {
  override def dependencies: Set[Contract] = Set(AddEqualityPrecedence, IntegerEqualsInstructionC)

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

  def getInputType(equality: MetaObject, state: TransformationState)  = {
    val first = getFirst(equality)
    ExpressionC.getType(state)(first)
  }

  override def toByteCode(equality: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val first = getFirst(equality)
    val second = getSecond(equality)
    val toInstructions = ExpressionC.getToInstructions(state)
    val inputType = TypeC.toStackType(getInputType(equality,state),state)
    val equalityInstructions: Seq[MetaObject] = inputType.clazz match {
      case LongTypeC.LongTypeKey => Seq(CompareLongC.compareLong, NotInstructionC.not)
      case IntTypeC.IntTypeKey => Seq(IntegerEqualsInstructionC.equals)
    }
    toInstructions(first) ++ toInstructions(second) ++ equalityInstructions
  }
}
