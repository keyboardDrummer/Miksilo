package transformations.bytecode.simpleBytecode

import core.exceptions.BadInputException
import core.transformation.MetaObject
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.InstructionTypeAnalysis.InstructionSideEffects
import transformations.types.ObjectTypeC

case class ProgramTypeState(stackTypes: Seq[MetaObject], variableTypes: Map[Int, MetaObject])

object InstructionTypeAnalysis {
  type InstructionSideEffects = Map[Int, MetaObject]  
}

abstract class InstructionTypeAnalysis(instructions: Seq[MetaObject])
  extends InstructionFlowAnalysis[ProgramTypeState](instructions) {

  def getSideEffects(typeState: ProgramTypeState, instruction: MetaObject): InstructionSideEffects
  def getSignature(typeState: ProgramTypeState, instruction: MetaObject): InstructionSignature
  
  case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
    override def toString = s"StackDoesNotFitInstructionInput: instruction= $instruction; inputTypes= $inputTypes; stack= $stack"
  }

  case class TargetInstructionEnteredWithDifferentLayouts(first: ProgramTypeState, second: ProgramTypeState) extends RuntimeException
  {
    override def toString = s"TargetInstructionEnteredWithDifferentLayouts, first:=$first, second:= $second"
  }

  override def combineState(first: ProgramTypeState, second: ProgramTypeState): Option[ProgramTypeState] = {
    if (first == second)
      return None

    throw new TargetInstructionEnteredWithDifferentLayouts(first, second)
  }

  override def updateState(state: ProgramTypeState, instructionIndex: Int): ProgramTypeState = {
    try
    {
      val stateStack = state.stackTypes
      val instruction = instructions(instructionIndex)
      val signature = getSignature(state, instruction)
      val input = signature.inputs
      if (input.length > stateStack.length)
        throw new StackDoesNotFitInstructionInput(instruction, input, stateStack)

      val stackTop = stateStack.takeRight(input.length)
      if (convertObjectTypesToObjectKey(input) != convertObjectTypesToObjectKey(stackTop))
        throw new StackDoesNotFitInstructionInput(instruction, input, stateStack)

      val remainingStack = stateStack.dropRight(input.length)
      val newStack = remainingStack ++ signature.outputs

      val updates = getSideEffects(state, instruction)
      val newVariables = updates ++ state.variableTypes
      ProgramTypeState(newStack, newVariables)
    }
    catch {
      case e: IndexOutOfBoundsException => throw MissingReturnInstruction
    }
  }

  def convertObjectTypesToObjectKey(input: Seq[MetaObject]): Seq[Object] = {
    input.map(_type => _type.clazz match {
      case ObjectTypeC.ObjectTypeKey => ObjectTypeC.ObjectTypeKey
      case _ => _type
    })
  }

  object MissingReturnInstruction extends BadInputException

}
