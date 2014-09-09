package transformations.javac.expressions.relational

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
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

  def getFirst(lessThan: MetaObject) = lessThan(LessThanFirst).asInstanceOf[MetaObject]

  def getSecond(lessThan: MetaObject) = lessThan(LessThanSecond).asInstanceOf[MetaObject]

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
    val parseLessThan = (relationalGrammar <~~ "<") ~~ relationalGrammar ^^ parseMap(LessThanKey, LessThanFirst, LessThanSecond)
    relationalGrammar.orToInner(parseLessThan)
  }

  def lessThan(first: MetaObject, second: MetaObject) = new MetaObject(LessThanKey, LessThanFirst -> first, LessThanSecond -> second)

  object LessThanKey

  object LessThanFirst

  object LessThanSecond

}
