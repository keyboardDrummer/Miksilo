package deltas.javac.expressions.additive

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.integers.SubtractIntegerDelta
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.additive.AdditionDelta.additionOrSubtractionConstraints
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ExpressionInstance, ToByteCodeSkeleton}

object SubtractionDelta extends ExpressionInstance with ConvertsToByteCodeDelta {
  object Shape extends NodeShape
  object Left extends NodeField
  object Right extends NodeField

  implicit class Subtraction[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def left: T = node(Left).asInstanceOf[T]
    def left_=(value: T): Unit = node(Left) = value

    def right: T = node(Right).asInstanceOf[T]
    def right_=(value: T): Unit = node(Right) = value
  }

  override def dependencies: Set[Contract] = Set(AdditivePrecedenceDelta, SubtractIntegerDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val additiveGrammar = find(AdditivePrecedenceDelta.Grammar)
    val withoutSubtraction = additiveGrammar.inner
    val parseSubtraction = additiveGrammar.as(Left) ~~< "-" ~~ withoutSubtraction.as(Right) asNode Shape
    additiveGrammar.addAlternative(parseSubtraction)
  }

  def subtraction(first: Any, second: Any): Node = subtraction(first.asInstanceOf[Node], second.asInstanceOf[Node])

  def subtraction(first: Node, second: Node) = new Node(Shape,
    Left -> first,
    Right -> second)

  override val shape = Shape

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    val subtraction: Subtraction[NodePath] = expression
    val getType = ExpressionDelta.getType(compilation)
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
