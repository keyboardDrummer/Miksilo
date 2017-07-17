package transformations.javac.expressions.relational

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.extraBooleanInstructions.{GreaterThanInstructionC, LessThanInstructionC}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.bytecode.types.{IntTypeC, TypeSkeleton}
import transformations.javac.types.BooleanTypeC

object GreaterThanC extends ExpressionInstance {

  val key = GreaterThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantC, LessThanInstructionC)

  override def toByteCode(lessThan: Path, state: CompilationState): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(GreaterThanInstructionC.greaterThanInstruction)
  }

  def getFirst[T <: NodeLike](lessThan: T) = lessThan(GreaterThanFirst).asInstanceOf[T]

  def getSecond[T <: NodeLike](lessThan: T) = lessThan(GreaterThanSecond).asInstanceOf[T]

  override def getType(expression: Path, state: CompilationState): Node = {
    val getType = ExpressionSkeleton.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, secondType)
    BooleanTypeC.booleanType
  }

  override def transformGrammars(grammars: GrammarCatalogue) {
    val relationalGrammar = grammars.find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar <~~ ">") ~~ relationalGrammar).asNode(GreaterThanKey, GreaterThanFirst, GreaterThanSecond)
    relationalGrammar.addOption(parseLessThan)
  }

  def lessThan(first: Node, second: Node) = new Node(GreaterThanKey, GreaterThanFirst -> first, GreaterThanSecond -> second)

  object GreaterThanKey extends Key

  object GreaterThanFirst extends Key

  object GreaterThanSecond extends Key

  override def description: String = "Adds the > operator."
}

object LessThanC extends ExpressionInstance {

  val key = LessThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, SmallIntegerConstantC, LessThanInstructionC)

  override def toByteCode(lessThan: Path, state: CompilationState): Seq[Node] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(LessThanInstructionC.lessThanInstruction)
  }

  def getFirst[T <: NodeLike](lessThan: T) = lessThan(LessThanFirst).asInstanceOf[T]

  def getSecond[T <: NodeLike](lessThan: T) = lessThan(LessThanSecond).asInstanceOf[T]

  override def getType(expression: Path, state: CompilationState): Node = {
    val getType = ExpressionSkeleton.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, secondType)
    BooleanTypeC.booleanType
  }

  override def transformGrammars(grammars: GrammarCatalogue) {
    val relationalGrammar = grammars.find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar <~~ "<") ~~ relationalGrammar).asNode(LessThanKey, LessThanFirst, LessThanSecond)
    relationalGrammar.addOption(parseLessThan)
  }

  def lessThan(first: Node, second: Node) = new Node(LessThanKey, LessThanFirst -> first, LessThanSecond -> second)

  object LessThanKey extends Key

  object LessThanFirst extends Key

  object LessThanSecond extends Key

  override def description: String = "Adds the < operator."
}
