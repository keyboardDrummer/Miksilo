package transformations.bytecode.extraBooleanInstructions

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.coreInstructions.integerCompare.IfNotZero.IfNotZeroKey
import transformations.bytecode.coreInstructions.integerCompare.IfZeroC.IfZeroKey
import transformations.bytecode.extraBooleanInstructions.LessThanInstructionC.LessThanInstructionKey
import transformations.bytecode.{ByteCodeSkeleton, LabelledJumps}

import scala.collection.mutable

object OptimizeBooleanInstructionsC extends ProgramTransformation {

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

      var i = 0
      while (i < instructions.size - 1) {
        val first = instructions(i)
        val second = instructions(i + 1)

        var instructionToAdd = first
        first.clazz match {
          case LessThanInstructionKey => second.clazz match {
            case IfZeroKey =>
              val target = LabelledJumps.getJumpInstructionLabel(second)
              instructionToAdd = LabelledJumps.ifIntegerCompareGreaterEquals(target)
              i += 1
            case IfNotZeroKey =>
              val target = LabelledJumps.getJumpInstructionLabel(second)
              instructionToAdd = LabelledJumps.ifIntegerCompareLess(target)
              i += 1
            case _ =>
          }
          case _ =>
        }
        newInstructions += instructionToAdd
        i += 1
      }
      if (i == instructions.size - 1)
        newInstructions += instructions(i)

      newInstructions
    }
  }
}
