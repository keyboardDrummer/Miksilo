package transformations.bytecode.extraBooleanInstructions

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.bytecode.{ByteCodeSkeleton, LabelledTargets}

import scala.collection.mutable

object ExpandInstructionsC extends ProgramTransformation {

  def lessThanInstruction = instruction(LessThanInstructionKey)

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  class State {
    val expandInstruction = new mutable.HashMap[Any, MetaObject => Seq[MetaObject]]()
  }

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

        val expandOption = getState(state).expandInstruction.get(instruction.clazz)

        val replacement = instruction.clazz match {
          case LessThanInstructionKey =>
            val falseStartLabel = state.getUniqueLabel("falseStart")
            val endLabel = state.getUniqueLabel("end")
            Seq(LabelledTargets.ifIntegerCompareLess(falseStartLabel),
              IntegerConstantC.integerConstant(0),
              LabelledTargets.goTo(endLabel),
              InferredStackFrames.label(falseStartLabel),
              IntegerConstantC.integerConstant(1),
              InferredStackFrames.label(endLabel))
          case _ => Seq(instruction)
        }
        newInstructions ++= expandOption.fold(Seq(instruction))(expand => expand(instruction))
      }

      newInstructions

    }
  }

  object LessThanInstructionKey

}
