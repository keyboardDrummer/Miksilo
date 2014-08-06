package transformations.javac.expressions

import core.grammar.{Grammar, seqr}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.instructions.IntegerConstantC
import transformations.bytecode.{ByteCodeSkeleton, InferredStackFrames, LabelledJumps}
import transformations.javac.types.{BooleanTypeC, IntTypeC, TypeC}

object LessThanC extends ExpressionInstance {

  val key = LessThanKey

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedence, IntegerConstantC)


  override def toByteCode(lessThan: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val toInstructions = ExpressionC.getToInstructions(state)
    val firstInstructions = toInstructions(getFirst(lessThan))
    val secondInstructions = toInstructions(getSecond(lessThan))
    val falseStartLabel = state.getUniqueLabel("falseStart")
    val endLabel = state.getUniqueLabel("end")
    firstInstructions ++ secondInstructions ++
      Seq(LabelledJumps.ifIntegerCompareGreater(falseStartLabel),
        IntegerConstantC.integerConstant(1),
        LabelledJumps.goTo(endLabel),
        InferredStackFrames.label(falseStartLabel),
        IntegerConstantC.integerConstant(0),
        InferredStackFrames.label(endLabel))
  }

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = {
    val getType = ExpressionC.getType(state)
    val firstType = getType(getFirst(expression))
    val secondType = getType(getSecond(expression))
    TypeC.checkAssignableTo(state)(IntTypeC.intType, firstType)
    TypeC.checkAssignableTo(state)(IntTypeC.intType, secondType)
    BooleanTypeC.booleanType
  }

  def getFirst(lessThan: MetaObject) = ByteCodeSkeleton.getInstructionArguments(lessThan)(0).asInstanceOf[MetaObject]

  def getSecond(lessThan: MetaObject) = ByteCodeSkeleton.getInstructionArguments(lessThan)(1).asInstanceOf[MetaObject]

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
