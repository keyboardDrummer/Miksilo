package transformations.bytecode.extraBooleanInstructions

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._

import scala.collection.mutable

object OptimizeBooleanInstructions extends ProgramTransformation {

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

      for (i <- 0.to(instructions.size - 1)) {
        val first = instructions(i)
        val second = instructions(i + 1)

        //        first.clazz match {
        //          case LessThanInstructionKey => second.clazz match {
        //            case IfIntegerCompareLessKey.key => {
        //
        //            }
        //          }
        //
        //        }
      }
      for (instruction <- instructions) {

        //        val replacement = instruction.clazz match {
        //            val falseStartLabel = state.getUniqueLabel("falseStart")
        //            val endLabel = state.getUniqueLabel("end")
        //            Seq(LabelledJumps.ifIntegerCompareGreater(falseStartLabel),
        //              IntegerConstantC.integerConstant(1),
        //              LabelledJumps.goTo(endLabel),
        //              InferredStackFrames.label(falseStartLabel),
        //              IntegerConstantC.integerConstant(0),
        //              InferredStackFrames.label(endLabel))
        //          case _ => Seq(instruction)
        //        }
        //        newInstructions ++= replacement
      }

      newInstructions

    }
  }
}
