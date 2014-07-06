package transformations.javac

import core.transformation.{TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base.JavaBase
import transformations.bytecode.{InferredStackFrames, LabelledJumps, ByteCode}

object LessThanC extends ProgramTransformation {
  object LessThanKey
  object LessThanFirst
  object LessThanSecond
  def lessThan(first: MetaObject, second: MetaObject) = ByteCode.instruction(LessThanKey, Seq(first,second))
  def getFirst(lessThan: MetaObject) = ByteCode.getInstructionArguments(lessThan)(0).asInstanceOf[MetaObject]
  def getSecond(lessThan: MetaObject) = ByteCode.getInstructionArguments(lessThan)(1).asInstanceOf[MetaObject]

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(LessThanKey,(subtraction : MetaObject, compiler) => {
      val firstInstructions = JavaBase.statementToInstructions(getFirst(subtraction), compiler)
      val secondInstructions = JavaBase.statementToInstructions(getSecond(subtraction), compiler)
      val falseStartLabel = state.getUniqueLabel("falseStart")
      val endLabel = state.getUniqueLabel("end")
      firstInstructions ++ secondInstructions ++
        Seq(LabelledJumps.ifIntegerCompareGreater(falseStartLabel),
          ByteCode.integerConstant(1),
          LabelledJumps.goTo(endLabel),
          InferredStackFrames.label(falseStartLabel),
          ByteCode.integerConstant(0),
          InferredStackFrames.label(endLabel))
    })
  }
}
