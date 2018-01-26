package deltas.javac.expressions.equality

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import core.language.Language
import deltas.bytecode.coreInstructions.longs.CompareLongDelta
import deltas.bytecode.extraBooleanInstructions.{IntegerEqualsInstructionDelta, NotInstructionDelta}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.bytecode.types.{IntTypeC, LongTypeC, TypeSkeleton}
import deltas.javac.types.BooleanTypeC

object EqualityDelta extends ExpressionInstance {
  override def dependencies: Set[Contract] = Set(AddEqualityPrecedence, IntegerEqualsInstructionDelta)

  def getFirst[T <: NodeLike](equality: T) = equality(FirstKey).asInstanceOf[T]

  def getSecond[T <: NodeLike](equality: T) = equality(SecondKey).asInstanceOf[T]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val equalityGrammar = find(AddEqualityPrecedence.EqualityExpressionGrammar)
    val parseEquality = ((equalityGrammar.as(FirstKey) ~< "==") ~ equalityGrammar.as(SecondKey)).asNode(EqualityKey)
    equalityGrammar.addOption(parseEquality)
  }

  def equality(first: Node, second: Node) = new Node(EqualityKey, FirstKey -> first, SecondKey -> second)

  object EqualityKey extends NodeShape

  object FirstKey extends NodeField

  object SecondKey extends NodeField

  override val key = EqualityKey

  override def getType(expression: Path, compilation: Compilation): Node = BooleanTypeC.booleanType

  def getInputType(equality: Path, compilation: Compilation) = {
    val first = getFirst(equality)
    ExpressionSkeleton.getType(compilation)(first)
  }

  override def toByteCode(equality: Path, compilation: Compilation): Seq[Node] = {
    val first = getFirst(equality)
    val second = getSecond(equality)
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val inputType = TypeSkeleton.toStackType(getInputType(equality, compilation), compilation)
    val equalityInstructions: Seq[Node] = inputType.shape match {
      case LongTypeC.LongTypeKey => Seq(CompareLongDelta.compareLong, NotInstructionDelta.not)
      case IntTypeC.IntTypeKey => Seq(IntegerEqualsInstructionDelta.equals)
    }
    toInstructions(first) ++ toInstructions(second) ++ equalityInstructions
  }

  override def description: String = "Adds the == operator."
}
