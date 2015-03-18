package transformations.javac.expressions.equality

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.path.Path
import transformations.bytecode.coreInstructions.longs.CompareLongC
import transformations.bytecode.extraBooleanInstructions.{IntegerEqualsInstructionC, NotInstructionC}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.types.{BooleanTypeC, IntTypeC, LongTypeC, TypeSkeleton}

object EqualityC extends ExpressionInstance {
  override def dependencies: Set[Contract] = Set(AddEqualityPrecedence, IntegerEqualsInstructionC)

  def getFirst[T <: MetaLike](equality: T) = equality(FirstKey).asInstanceOf[T]

  def getSecond[T <: MetaLike](equality: T) = equality(SecondKey).asInstanceOf[T]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val equalityGrammar = grammars.find(AddEqualityPrecedence.EqualityExpressionGrammar)
    val parseEquality = (equalityGrammar <~ "==") ~ equalityGrammar ^^ parseMap(EqualityKey, FirstKey, SecondKey)
    equalityGrammar.addOption(parseEquality)
  }

  def equality(first: MetaObject, second: MetaObject) = new MetaObject(EqualityKey, FirstKey -> first, SecondKey -> second)

  object EqualityKey

  object FirstKey

  object SecondKey

  override val key: AnyRef = EqualityKey

  override def getType(expression: Path, state: CompilationState): MetaObject = BooleanTypeC.booleanType

  def getInputType(equality: Path, state: CompilationState)  = {
    val first = getFirst(equality)
    ExpressionSkeleton.getType(state)(first)
  }

  override def toByteCode(equality: Path, state: CompilationState): Seq[MetaObject] = {
    val first = getFirst(equality)
    val second = getSecond(equality)
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val inputType = TypeSkeleton.toStackType(getInputType(equality,state),state)
    val equalityInstructions: Seq[MetaObject] = inputType.clazz match {
      case LongTypeC.LongTypeKey => Seq(CompareLongC.compareLong, NotInstructionC.not)
      case IntTypeC.IntTypeKey => Seq(IntegerEqualsInstructionC.equals)
    }
    toInstructions(first) ++ toInstructions(second) ++ equalityInstructions
  }

  override def description: String = "Adds the == operator."
}
