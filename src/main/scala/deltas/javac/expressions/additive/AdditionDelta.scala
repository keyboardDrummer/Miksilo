package deltas.javac.expressions.additive

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import core.language.Language
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import core.smarts.{ConstraintBuilder, ConstraintSolver}
import deltas.bytecode.coreInstructions.integers.AddIntegersDelta
import deltas.bytecode.coreInstructions.longs.AddLongsDelta
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, TypeSkeleton}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object AdditionDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the + operator."

  val key = Shape

  override def toByteCode(addition: Path, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(addition.left)
    val secondInstructions = toInstructions(addition.right)
    firstInstructions ++ secondInstructions ++ (getType(addition, compilation) match {
      case x if x == IntTypeDelta.intType => Seq(AddIntegersDelta.addIntegers())
      case x if x == LongTypeDelta.longType => Seq(AddLongsDelta.addLongs())
      case _ => throw new NotImplementedError()
    })
  }

  override def getType(expression: Path, compilation: Compilation): Node = {
    val getType = ExpressionSkeleton.getType(compilation)
    val firstType = getType(expression.left)
    val secondType = getType(expression.right)
    firstType match
    {
      case x if x == IntTypeDelta.intType =>
        TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, firstType)
        TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, secondType)
        IntTypeDelta.intType
      case x if x == LongTypeDelta.longType =>
        TypeSkeleton.checkAssignableTo(compilation)(LongTypeDelta.longType, firstType)
        TypeSkeleton.checkAssignableTo(compilation)(LongTypeDelta.longType, secondType)
        LongTypeDelta.longType
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

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: Path, _type: Type, parentScope: Scope): Unit = {
    val firstType = ExpressionSkeleton.getType(compilation, builder, expression.left, parentScope)
    val secondType = ExpressionSkeleton.getType(compilation, builder, expression.right, parentScope)
    builder.add((solver: ConstraintSolver) => {
      val resolved = solver.resolveType(firstType)
      val additionTypeOption: Option[Type] = resolved match {
        case IntTypeDelta.constraintType => Some(IntTypeDelta.constraintType)
        case LongTypeDelta.constraintType => Some(LongTypeDelta.constraintType)
        case _ => None
      }
      additionTypeOption.fold(false)(additionType => {
        builder.typesAreEqual(additionType, secondType)
        builder.typesAreEqual(additionType, _type)
        true
      })
    })
  }
}
