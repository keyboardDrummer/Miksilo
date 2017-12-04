package deltas.bytecode.simpleBytecode

import core.deltas.exceptions.BadInputException
import core.deltas.node.Node
import deltas.bytecode.coreInstructions.InstructionDelta.Instruction
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.InstructionTypeAnalysis.InstructionSideEffects
import deltas.bytecode.types.ObjectTypeDelta

case class ProgramTypeState(stackTypes: Seq[Node], variableTypes: Map[Int, Node])

object InstructionTypeAnalysis {
  type InstructionSideEffects = Map[Int, Node]
}

abstract class InstructionTypeAnalysis(instructions: Seq[Instruction[Node]])
  extends InstructionFlowAnalysis[ProgramTypeState](instructions) {

  def getSideEffects(typeState: ProgramTypeState, instruction: Instruction[Node]): InstructionSideEffects
  def getSignature(typeState: ProgramTypeState, instruction: Instruction[Node]): InstructionSignature
  
  case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
    override def toString = s"StackDoesNotFitInstructionInput: instruction = $instruction; inputTypes = $inputTypes; stack = $stack"
  }

  case class TargetInstructionEnteredWithDifferentLayouts(first: ProgramTypeState, second: ProgramTypeState) extends RuntimeException
  {
    override def toString = s"TargetInstructionEnteredWithDifferentLayouts, first:=$first, second:= $second"
  }

  override def combineState(first: ProgramTypeState, second: ProgramTypeState): Option[ProgramTypeState] = {
    if (first.stackTypes != second.stackTypes)
      throw new TargetInstructionEnteredWithDifferentLayouts(first, second)

    val firstVariables: Map[Int, Node] = first.variableTypes
    val secondVariables: Map[Int, Node] = second.variableTypes
    if (firstVariables == secondVariables)
      return None

    val sharedKeys: Set[Int] = firstVariables.keySet.intersect(secondVariables.keySet)
    val newVariables: Map[Int, Node] = sharedKeys.map(key => {
      val firstValue: Node = firstVariables(key)
      if (firstValue != secondVariables(key))
        throw new TargetInstructionEnteredWithDifferentLayouts(first, second)

      (key, firstValue)
    }).toMap
    Some(new ProgramTypeState(first.stackTypes, newVariables))
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

  def convertObjectTypesToObjectKey(input: Seq[Node]): Seq[Object] = {
    input.map(_type => _type.clazz match {
      case ObjectTypeDelta.ObjectTypeKey => ObjectTypeDelta.ObjectTypeKey
      case _ => _type
    })
  }

  object MissingReturnInstruction extends BadInputException

}
