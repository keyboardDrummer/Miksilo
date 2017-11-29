package deltas.bytecode.coreInstructions

import core.deltas.{Compilation, Language}
import core.deltas.node.{Key, Node, NodeClass}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.simpleBytecode.ProgramTypeState

object InvokeVirtualDelta extends InvokeDelta {

  override val key = InvokeVirtual

  def invokeVirtual(methodRefIndex: Any) = InvokeVirtual.create(MethodRef -> methodRefIndex)

  override def getInstructionSize: Int = 3
  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b6") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    getInstanceInstructionSignature(instruction, typeState, language)
  }

  object InvokeVirtual extends NodeClass

  override def description: String = "Defines the invoke virtual instruction, which can be used to call virtual methods."

  override def grammarName = "invokevirtual"
}
