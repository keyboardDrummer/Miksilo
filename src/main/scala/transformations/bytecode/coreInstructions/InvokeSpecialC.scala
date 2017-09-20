package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.{Key, Node, NodeClass}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.simpleBytecode.ProgramTypeState

/**
 * Invokes an instance method using static binding, so no dynamic dispatch is applied.
 */
object InvokeSpecialC extends InvokeC {
  override val key: Key = InvokeSpecialKey

  def invokeSpecial(location: Any): Node = InvokeSpecialKey.create(MethodRef -> location)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b7") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  override def getInstructionSize: Int = 3
  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    getInstanceInstructionSignature(instruction, typeState, state)
  }

  object InvokeSpecialKey extends NodeClass

  override def description: String = "Defines the invoke special method, which can be used to call constructors."
}