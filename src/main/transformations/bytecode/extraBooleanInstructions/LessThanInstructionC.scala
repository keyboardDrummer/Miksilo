package transformations.bytecode.extraBooleanInstructions

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.instructions.IntegerConstantC
import transformations.bytecode.instructions.integerCompare.IfIntegerCompareLessC
import transformations.bytecode.{ByteCodeSkeleton, InferredStackFrames, LabelledJumps}

import scala.collection.mutable

object LessThanInstructionC extends ProgramTransformation {

  def lessThanInstruction = instruction(LessThanInstructionKey)

  override def dependencies: Set[Contract] = Set(LabelledJumps, IfIntegerCompareLessC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

    val clazz = program
    val codeAnnotations: Seq[MetaObject] = getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: MetaObject): Option[Any] = {
      val instructions = ByteCodeSkeleton.getCodeInstructions(codeAnnotation)
      val newInstructions: Seq[MetaObject] = getNewInstructions(instructions)
      codeAnnotation(ByteCodeSkeleton.CodeInstructionsKey) = newInstructions
    }

    def getNewInstructions(instructions: Seq[MetaObject]) = {

      var newInstructions = mutable.ArrayBuffer[MetaObject]()

      for (instruction <- instructions) {

        val replacement = instruction.clazz match {
          case LessThanInstructionKey =>
            val falseStartLabel = state.getUniqueLabel("falseStart")
            val endLabel = state.getUniqueLabel("end")
            Seq(LabelledJumps.ifIntegerCompareLess(falseStartLabel),
              IntegerConstantC.integerConstant(0),
              LabelledJumps.goTo(endLabel),
              InferredStackFrames.label(falseStartLabel),
              IntegerConstantC.integerConstant(1),
              InferredStackFrames.label(endLabel))
          case _ => Seq(instruction)
        }
        newInstructions ++= replacement
      }

      newInstructions

    }
  }

  object LessThanInstructionKey

}
