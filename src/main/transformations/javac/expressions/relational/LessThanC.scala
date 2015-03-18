package transformations.javac.expressions.relational

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.bytecode.extraBooleanInstructions.LessThanInstructionC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.types.{BooleanTypeC, IntTypeC, TypeSkeleton}

object LessThanC extends ExpressionInstance {

  val key = LessThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, IntegerConstantC, LessThanInstructionC)

  override def toByteCode(lessThan: Path, state: CompilationState): Seq[MetaObject] = {
    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(LessThanInstructionC.lessThanInstruction)
  }

  def getFirst[T <: MetaLike](lessThan: T) = lessThan(LessThanFirst).asInstanceOf[T]

  def getSecond[T <: MetaLike](lessThan: T) = lessThan(LessThanSecond).asInstanceOf[T]

  override def getType(expression: Path, state: CompilationState): MetaObject = {
    val getType = ExpressionSkeleton.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeSkeleton.checkAssignableTo(state)(IntTypeC.intType, secondType)
    BooleanTypeC.booleanType
  }

  override def transformGrammars(grammars: GrammarCatalogue) {
    val relationalGrammar = grammars.find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan = (relationalGrammar <~~ "<") ~~ relationalGrammar ^^ parseMap(LessThanKey, LessThanFirst, LessThanSecond)
    relationalGrammar.addOption(parseLessThan)
  }

  def lessThan(first: MetaObject, second: MetaObject) = new MetaObject(LessThanKey, LessThanFirst -> first, LessThanSecond -> second)

  object LessThanKey

  object LessThanFirst

  object LessThanSecond

  override def description: String = "Adds the < operator."
}
