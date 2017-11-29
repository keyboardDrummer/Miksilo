package deltas.bytecode.coreInstructions

import core.deltas.{Compilation, Language}
import core.deltas.node.{Key, Node, NodeClass}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.simpleBytecode.ProgramTypeState

/**
 * Invokes an instance method using static binding, so no dynamic dispatch is applied.
 */
object InvokeSpecialDelta extends InvokeDelta {
  override val key = InvokeSpecialKey

  def invokeSpecial(location: Any): Node = InvokeSpecialKey.create(MethodRef -> location)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b7") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  override def getInstructionSize: Int = 3
  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    getInstanceInstructionSignature(instruction, typeState, language)
  }

  object InvokeSpecialKey extends NodeClass

  override def description: String = "Defines the invoke special method, which can be used to call constructors."

  override def grammarName = "invokespecial"
}