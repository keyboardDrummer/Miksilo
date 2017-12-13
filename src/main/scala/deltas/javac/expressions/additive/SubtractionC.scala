package deltas.javac.expressions.additive

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import deltas.bytecode.coreInstructions.integers.SubtractIntegerDelta
import deltas.bytecode.types.{IntTypeC, TypeSkeleton}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object SubtractionC extends ExpressionInstance {
  object SubtractionKey extends NodeShape
  object FirstKey extends NodeField
  object SecondKey extends NodeField

  def getFirst[T <: NodeLike](subtraction: T) = subtraction(FirstKey).asInstanceOf[T]

  def getSecond[T <: NodeLike](subtraction: T) = subtraction(SecondKey).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, SubtractIntegerDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val additiveGrammar = find(AddAdditivePrecedence.Grammar)
    val withoutSubtraction = additiveGrammar.inner //We're doing this to get "-" to behave right associative. Hope this doesn't have any bad side-effects.
    val parseSubtraction = (additiveGrammar.as(FirstKey) ~~< "-") ~~ withoutSubtraction.as(SecondKey) asNode SubtractionKey
    additiveGrammar.addOption(parseSubtraction)
  }

  def subtraction(first: Any, second: Any): Node = subtraction(first.asInstanceOf[Node], second.asInstanceOf[Node])

  def subtraction(first: Node, second: Node) = new Node(SubtractionKey,
    FirstKey -> first,
    SecondKey -> second)

  override val key = SubtractionKey

  override def getType(expression: Path, compilation: Compilation): Node = {
    val getType = ExpressionSkeleton.getType(compilation)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, secondType)
    IntTypeC.intType
  }

  override def toByteCode(subtraction: Path, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(getFirst(subtraction))
    val secondInstructions = toInstructions(getSecond(subtraction))
    firstInstructions ++ secondInstructions ++ Seq(SubtractIntegerDelta.subtractInteger)
  }

  override def description: String = "Adds the - operator."
}
