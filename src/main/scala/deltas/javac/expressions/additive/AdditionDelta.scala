package deltas.javac.expressions.additive

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import core.language.Language
import deltas.bytecode.coreInstructions.integers.AddIntegersDelta
import deltas.bytecode.coreInstructions.longs.AddLongsDelta
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.bytecode.types.{IntTypeC, LongTypeC, TypeSkeleton}

object AdditionDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the + operator."

  val key = Shape

  override def toByteCode(addition: Path, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(addition.left)
    val secondInstructions = toInstructions(addition.right)
    firstInstructions ++ secondInstructions ++ (getType(addition, compilation) match {
      case x if x == IntTypeC.intType => Seq(AddIntegersDelta.addIntegers())
      case x if x == LongTypeC.longType => Seq(AddLongsDelta.addLongs())
      case _ => throw new NotImplementedError()
    })
  }

  override def getType(expression: Path, compilation: Compilation): Node = {
    val getType = ExpressionSkeleton.getType(compilation)
    val firstType = getType(expression.left)
    val secondType = getType(expression.right)
    firstType match
    {
      case x if x == IntTypeC.intType =>
        TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, firstType)
        TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, secondType)
        IntTypeC.intType
      case x if x == LongTypeC.longType =>
        TypeSkeleton.checkAssignableTo(compilation)(LongTypeC.longType, firstType)
        TypeSkeleton.checkAssignableTo(compilation)(LongTypeC.longType, secondType)
        LongTypeC.longType
      case _ => throw new NotImplementedError()
    }
  }

  override def dependencies: Set[Contract] = Set(AddAdditivePrecedence, AddIntegersDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val additiveGrammar = find(AddAdditivePrecedence.Grammar)
    val parseAddition = additiveGrammar.as(Left) ~~< "+" ~~ additiveGrammar.as(Right) asNode Shape
    additiveGrammar.addOption(parseAddition)
  }

implicit class Addition[T <: NodeLike](val node: T) extends NodeWrapper[T] {
  def left: T = node(Left).asInstanceOf[T]
  def left_=(value: T): Unit = node(Left) = value

  def right: T = node(Right).asInstanceOf[T]
  def right_=(value: T): Unit = node(Right) = value
}

  def addition(first: Node, second: Node) = new Node(Shape, Left -> first, Right -> second)

  object Shape extends NodeShape

  object Left extends NodeField
  object Right extends NodeField
}
