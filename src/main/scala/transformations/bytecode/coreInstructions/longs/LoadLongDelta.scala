package transformations.bytecode.coreInstructions.longs

import core.particles.Compilation
import core.particles.node.{Node, NodeClass}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.LongTypeC

object LoadLongDelta extends InstructionDelta {

  override val key = LongLoad

  def load(location: Integer) = CodeAttribute.instruction(LongLoad, Seq(location))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("16") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("1e") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(), Seq(LongTypeC.longType))

  object LongLoad extends NodeClass

  override def grammarName = "lload" //TODO lload_0 etc..
}

