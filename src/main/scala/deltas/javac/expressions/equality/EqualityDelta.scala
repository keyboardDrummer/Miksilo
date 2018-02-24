package deltas.javac.expressions.equality

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.longs.CompareLongDelta
import deltas.bytecode.extraBooleanInstructions.{IntegerEqualsInstructionDelta, NotInstructionDelta}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, TypeSkeleton}
import deltas.javac.types.BooleanTypeDelta

object EqualityDelta extends ExpressionInstance {
  override def dependencies: Set[Contract] = Set(AddEqualityPrecedence, IntegerEqualsInstructionDelta)

  def getFirst[T <: NodeLike](equality: T): T = equality(FirstKey).asInstanceOf[T]

  def getSecond[T <: NodeLike](equality: T): T = equality(SecondKey).asInstanceOf[T]

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

  override val shape = EqualityKey

  override def getType(expression: NodePath, compilation: Compilation): Node = BooleanTypeDelta.booleanType

  def getInputType(equality: NodePath, compilation: Compilation) = {
    val first = getFirst(equality)
    ExpressionSkeleton.getType(compilation)(first)
  }

  override def toByteCode(equality: NodePath, compilation: Compilation): Seq[Node] = {
    val first = getFirst(equality)
    val second = getSecond(equality)
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val inputType = TypeSkeleton.toStackType(getInputType(equality, compilation), compilation)
    val equalityInstructions: Seq[Node] = inputType.shape match {
      case LongTypeDelta.LongTypeKey => Seq(CompareLongDelta.compareLong, NotInstructionDelta.not)
      case IntTypeDelta.IntTypeKey => Seq(IntegerEqualsInstructionDelta.equals)
    }
    toInstructions(first) ++ toInstructions(second) ++ equalityInstructions
  }

  override def description: String = "Adds the == operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    //TODO add a check for first and secondType.
//    val firstType = ExpressionSkeleton.getType(compilation, builder, getFirst(expression), parentScope)
//    val secondType = ExpressionSkeleton.getType(compilation, builder, getSecond(expression), parentScope)
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }
}
