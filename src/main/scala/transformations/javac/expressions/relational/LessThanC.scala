package transformations.javac.expressions.relational

import core.particles._
import core.particles.grammars.LanguageGrammars
import core.particles.node._
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import transformations.bytecode.extraBooleanInstructions.{GreaterThanInstructionC, LessThanInstructionC}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.bytecode.types.{IntTypeC, TypeSkeleton}
import transformations.javac.types.BooleanTypeC

object GreaterThanC extends ExpressionInstance {

  val key = GreaterThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantDelta, LessThanInstructionC)

  override def toByteCode(lessThan: Path, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(GreaterThanInstructionC.greaterThanInstruction)
  }

  def getFirst[T <: NodeLike](lessThan: T) = lessThan(GreaterThanFirst).asInstanceOf[T]

  def getSecond[T <: NodeLike](lessThan: T) = lessThan(GreaterThanSecond).asInstanceOf[T]

  override def getType(expression: Path, compilation: Compilation): Node = {
    val getType = ExpressionSkeleton.getType(compilation)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, secondType)
    BooleanTypeC.booleanType
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val relationalGrammar = find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar.as(GreaterThanFirst) ~~< ">") ~~ relationalGrammar.as(GreaterThanSecond)).asNode(GreaterThanKey)
    relationalGrammar.addOption(parseLessThan)
  }

  def lessThan(first: Node, second: Node) = new Node(GreaterThanKey, GreaterThanFirst -> first, GreaterThanSecond -> second)

  object GreaterThanKey extends NodeClass

  object GreaterThanFirst extends NodeField

  object GreaterThanSecond extends NodeField

  override def description: String = "Adds the > operator."
}

object LessThanC extends ExpressionInstance {

  val key = LessThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantDelta, LessThanInstructionC)

  override def toByteCode(lessThan: Path, compilation: Compilation): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(LessThanInstructionC.lessThanInstruction)
  }

  def getFirst[T <: NodeLike](lessThan: T) = lessThan(LessThanFirst).asInstanceOf[T]

  def getSecond[T <: NodeLike](lessThan: T) = lessThan(LessThanSecond).asInstanceOf[T]

  override def getType(expression: Path, compilation: Compilation): Node = {
    val getType = ExpressionSkeleton.getType(compilation)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeC.intType, secondType)
    BooleanTypeC.booleanType
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val relationalGrammar = find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar.as(LessThanFirst) ~~< "<") ~~ relationalGrammar.as(LessThanSecond)).asNode(LessThanKey)
    relationalGrammar.addOption(parseLessThan)
  }

  def lessThan(first: Node, second: Node) = new Node(LessThanKey, LessThanFirst -> first, LessThanSecond -> second)

  object LessThanKey extends NodeClass

  object LessThanFirst extends NodeField

  object LessThanSecond extends NodeField

  override def description: String = "Adds the < operator."
}
