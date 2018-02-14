package deltas.javac.expressions.relational

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.extraBooleanInstructions.{GreaterThanInstructionDelta, LessThanInstructionDelta}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.javac.types.BooleanTypeDelta

object LessThanDelta extends ExpressionInstance {

  val shape = LessThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantDelta, LessThanInstructionDelta)

  override def toByteCode(lessThan: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(LessThanInstructionDelta.lessThanInstruction)
  }

  def getFirst[T <: NodeLike](lessThan: T) = lessThan(LessThanFirst).asInstanceOf[T]

  def getSecond[T <: NodeLike](lessThan: T) = lessThan(LessThanSecond).asInstanceOf[T]

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
    val parseLessThan = ((relationalGrammar.as(LessThanFirst) ~~< "<") ~~ relationalGrammar.as(LessThanSecond)).asNode(LessThanKey)
    relationalGrammar.addOption(parseLessThan)
  }

  def lessThan(first: Node, second: Node) = new Node(LessThanKey, LessThanFirst -> first, LessThanSecond -> second)

  object LessThanKey extends NodeShape

  object LessThanFirst extends NodeField

  object LessThanSecond extends NodeField

  override def description: String = "Adds the < operator."
}
