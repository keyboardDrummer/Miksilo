package deltas.javac.expressions.relational

import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeLike, NodeShape}
import core.deltas.path.NodePath
import core.deltas.Contract
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.extraBooleanInstructions.{GreaterThanInstructionDelta, LessThanInstructionDelta}
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.types.BooleanTypeDelta

object GreaterThanDelta extends ExpressionInstance {

  override def description: String = "Adds the > operator."

  val shape = GreaterThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantDelta, LessThanInstructionDelta)

  override def toByteCode(lessThan: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(GreaterThanInstructionDelta.greaterThanInstruction)
  }

  def getFirst[T <: NodeLike](lessThan: T) = lessThan(GreaterThanFirst).asInstanceOf[T]

  def getSecond[T <: NodeLike](lessThan: T) = lessThan(GreaterThanSecond).asInstanceOf[T]

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    val getType = ExpressionSkeleton.getType(compilation)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, secondType)
    BooleanTypeDelta.booleanType
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val relationalGrammar = find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar.as(GreaterThanFirst) ~~< ">") ~~ relationalGrammar.as(GreaterThanSecond)).asNode(GreaterThanKey)
    relationalGrammar.addOption(parseLessThan)
  }

  def lessThan(first: Node, second: Node) = new Node(GreaterThanKey, GreaterThanFirst -> first, GreaterThanSecond -> second)

  object GreaterThanKey extends NodeShape

  object GreaterThanFirst extends NodeField

  object GreaterThanSecond extends NodeField

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    //TODO add a check for first and secondType. Share code with other comparisons.
    //    val firstType = ExpressionSkeleton.getType(compilation, builder, getFirst(expression), parentScope)
    //    val secondType = ExpressionSkeleton.getType(compilation, builder, getSecond(expression), parentScope)
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }
}
