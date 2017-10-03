package transformations.javac.expressions.equality

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.longs.CompareLongDelta
import transformations.bytecode.extraBooleanInstructions.{IntegerEqualsInstructionC, NotInstructionC}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.bytecode.types.{IntTypeC, LongTypeC, TypeSkeleton}
import transformations.javac.types.BooleanTypeC

object EqualityC extends ExpressionInstance {
  override def dependencies: Set[Contract] = Set(AddEqualityPrecedence, IntegerEqualsInstructionC)

  def getFirst[T <: NodeLike](equality: T) = equality(FirstKey).asInstanceOf[T]

  def getSecond[T <: NodeLike](equality: T) = equality(SecondKey).asInstanceOf[T]

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val equalityGrammar = grammars.find(AddEqualityPrecedence.EqualityExpressionGrammar)
    val parseEquality = ((equalityGrammar <~ "==") ~ equalityGrammar).asNode(EqualityKey, FirstKey, SecondKey)
    equalityGrammar.addOption(parseEquality)
  }

  def equality(first: Node, second: Node) = new Node(EqualityKey, FirstKey -> first, SecondKey -> second)

  object EqualityKey extends Key

  object FirstKey extends Key

  object SecondKey extends Key

  override val key: Key = EqualityKey

  override def getType(expression: Path, state: Language): Node = BooleanTypeC.booleanType

  def getInputType(equality: Path, state: Language)  = {
    val first = getFirst(equality)
    ExpressionSkeleton.getType(state)(first)
  }

  override def toByteCode(equality: Path, state: Language): Seq[Node] = {
    val first = getFirst(equality)
    val second = getSecond(equality)
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val inputType = TypeSkeleton.toStackType(getInputType(equality,state),state)
    val equalityInstructions: Seq[Node] = inputType.clazz match {
      case LongTypeC.LongTypeKey => Seq(CompareLongDelta.compareLong, NotInstructionC.not)
      case IntTypeC.IntTypeKey => Seq(IntegerEqualsInstructionC.equals)
    }
    toInstructions(first) ++ toInstructions(second) ++ equalityInstructions
  }

  override def description: String = "Adds the == operator."
}
