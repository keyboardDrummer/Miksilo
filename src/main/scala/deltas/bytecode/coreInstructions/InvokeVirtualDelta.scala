package deltas.bytecode.coreInstructions

import core.deltas.Language
import core.deltas.node.Node
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.simpleBytecode.ProgramTypeState

object InvokeVirtualDelta extends InvokeDelta {

  def invokeVirtual(methodRefIndex: Any) = key.create(MethodRef -> methodRefIndex)

  override def getInstructionSize: Int = 3
  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b6") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    getInstanceInstructionSignature(instruction, typeState, language)
  }

  override def description: String = "Defines the invoke virtual instruction, which can be used to call virtual methods."

  override def grammarName = "invokevirtual"
}
