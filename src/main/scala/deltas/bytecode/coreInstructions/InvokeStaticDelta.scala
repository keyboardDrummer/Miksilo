package deltas.bytecode.coreInstructions

import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode._

object InvokeStaticDelta extends InvokeDelta {

  def invokeStatic(constantIndex: Any): Node = key.create(MethodRef -> constantIndex)

  override def getInstructionSize: Int = 3
  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b8") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  override def description: String = "Defines the invoke static instruction, which can be used to call static methods."

  override def grammarName = "invokestatic"
}
