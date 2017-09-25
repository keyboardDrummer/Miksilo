package transformations.bytecode.coreInstructions.integers

import core.particles.node.{Key, Node}
import core.particles.{Compilation, Contract, Language}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object LoadIntegerDelta extends InstructionDelta {

  override val key: Key = IntegerLoad

  def load(location: Integer) = CodeAttribute.instruction(IntegerLoad, Seq(location))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments.head
    if (location > 3)
      hexToBytes("15") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("1a") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeC.intType))

  object IntegerLoad extends Key

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the load integer instruction, which loads an integer from a variable."

  override def grammarName = "iload" //TODO eigenlijk heb je ook nog iload_0 etc.. maar die zitten verbogen in deze Delta.
}
