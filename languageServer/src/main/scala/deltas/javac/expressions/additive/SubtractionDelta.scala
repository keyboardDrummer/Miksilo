package deltas.javac.expressions.additive

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.integers.SubtractIntegerDelta
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.javac.expressions.additive.AdditionDelta.{Left, Right, additionOrSubtractionConstraints}
import deltas.javac.expressions.{ConvertsToByteCode, ExpressionInstance, ExpressionSkeleton, ToByteCodeSkeleton}

object SubtractionDelta extends ExpressionInstance with ConvertsToByteCode {
  object SubtractionKey extends NodeShape
  object FirstKey extends NodeField
  object SecondKey extends NodeField

  implicit class Subtraction[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def left: T = node(FirstKey).asInstanceOf[T]
    def left_=(value: T): Unit = node(Left) = value

    def right: T = node(SecondKey).asInstanceOf[T]
    def right_=(value: T): Unit = node(Right) = value
  }

  override def dependencies: Set[Contract] = Set(AdditivePrecedenceDelta, SubtractIntegerDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val additiveGrammar = find(AdditivePrecedenceDelta.Grammar)
    val withoutSubtraction = additiveGrammar.inner //We're doing this to get "-" to behave right associative. Hope this doesn't have any bad side-effects.
    val parseSubtraction = (additiveGrammar.as(FirstKey) ~~< "-") ~~ withoutSubtraction.as(SecondKey) asNode SubtractionKey
    additiveGrammar.addAlternative(parseSubtraction)
  }

  def subtraction(first: Any, second: Any): Node = subtraction(first.asInstanceOf[Node], second.asInstanceOf[Node])

  def subtraction(first: Node, second: Node) = new Node(SubtractionKey,
    FirstKey -> first,
    SecondKey -> second)

  override val shape = SubtractionKey

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    val subtraction: Subtraction[NodePath] = expression
    val getType = ExpressionSkeleton.getType(compilation)
    val firstType = getType(subtraction.left)
    val secondType = getType(subtraction.right)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, secondType)
    IntTypeDelta.intType
  }

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    val subtraction: Subtraction[NodePath] = expression
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(subtraction.left)
    val secondInstructions = toInstructions(subtraction.right)
    firstInstructions ++ secondInstructions ++ Seq(SubtractIntegerDelta.subtractInteger)
  }

  override def description: String = "Adds the - operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val left = expression.left
    val right = expression.right
    additionOrSubtractionConstraints(compilation, builder, _type, parentScope, left, right)
  }
}
