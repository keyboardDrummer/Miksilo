package deltas.expression.additive

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import core.smarts.{ConstraintBuilder, ConstraintSolver}
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, TypeSkeleton}
import deltas.expression.{ExpressionDelta, ExpressionInstance}

object AdditionDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the + operator."

  val shape = Shape

  override def dependencies: Set[Contract] = Set(AdditivePrecedenceDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._

    val additiveGrammar = find(AdditivePrecedenceDelta.Grammar)
    val parseAddition = additiveGrammar.as(Left) ~~< "+" ~~ additiveGrammar.as(Right) asNode Shape
    additiveGrammar.addAlternative(parseAddition)
  }

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    val getType = ExpressionDelta.getType(compilation)
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

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val left = expression.left
    val right = expression.right
    additionOrSubtractionConstraints(compilation, builder, _type, parentScope, left, right)
  }

  def additionOrSubtractionConstraints(compilation: Compilation, builder: ConstraintBuilder, _type: Type, parentScope: Scope, left: NodePath, right: NodePath): Unit = {
    val firstType = ExpressionDelta.getType(compilation, builder, left, parentScope)
    val secondType = ExpressionDelta.getType(compilation, builder, right, parentScope)
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
