package transformations.javac.expressions.relational

import core.grammar.{Grammar, seqr}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.coreInstructions.IntegerConstantC
import transformations.bytecode.extraBooleanInstructions.LessThanInstructionC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.{BooleanTypeC, IntTypeC, TypeC}

object LessThanC extends ExpressionInstance {

  val key = LessThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, IntegerConstantC, LessThanInstructionC)

  override def toByteCode(lessThan: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val toInstructions = ExpressionC.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    firstInstructions ++ secondInstructions ++ Seq(LessThanInstructionC.lessThanInstruction)
  }

  def getFirst(lessThan: MetaObject) = ByteCodeSkeleton.getInstructionArguments(lessThan)(0).asInstanceOf[MetaObject]

  def getSecond(lessThan: MetaObject) = ByteCodeSkeleton.getInstructionArguments(lessThan)(1).asInstanceOf[MetaObject]

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = {
    val getType = ExpressionC.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeC.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeC.checkAssignableTo(state)(IntTypeC.intType, secondType)
    BooleanTypeC.booleanType
  }

  override def transformGrammars(grammars: GrammarCatalogue) {
    val relationalGrammar = grammars.find(AddRelationalPrecedence.RelationalExpressionGrammar)
    val parseLessThan: Grammar = (relationalGrammar <~ "<") ~ relationalGrammar ^^ { case left seqr right => lessThan(left, right)}
    relationalGrammar.inner = relationalGrammar.inner | parseLessThan
  }

  private def lessThan(left: Any, right: Any): MetaObject = lessThan(left.asInstanceOf[MetaObject], right.asInstanceOf[MetaObject])

  def lessThan(first: MetaObject, second: MetaObject) = ByteCodeSkeleton.instruction(LessThanKey, Seq(first, second))

  object LessThanKey

  object LessThanFirst

  object LessThanSecond

}
