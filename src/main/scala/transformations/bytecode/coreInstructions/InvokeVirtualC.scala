package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.simpleBytecode.ProgramTypeState

object InvokeVirtualC extends InvokeC {

  override val key: Key = InvokeVirtual

  def invokeVirtual(methodRefIndex: Int) = CodeAttribute.instruction(InvokeVirtual, Seq(methodRefIndex))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("b6") ++ shortToBytes(arguments(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    getInstanceInstructionSignature(instruction, typeState, state)
  }

  object InvokeVirtual extends Key

  override def description: String = "Defines the invoke virtual instruction, which can be used to call virtual methods."
}
