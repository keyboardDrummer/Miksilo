package deltas.bytecode.coreInstructions

import core.particles.node.{Key, Node, NodeClass}
import deltas.bytecode.PrintByteCode._

object InvokeStaticDelta extends InvokeDelta {

  override val key = InvokeStaticKey

  def invokeStatic(constantIndex: Any): Node = InvokeStaticKey.create(MethodRef -> constantIndex)

  override def getInstructionSize: Int = 3
  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b8") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  object InvokeStaticKey extends NodeClass

  override def description: String = "Defines the invoke static instruction, which can be used to call static methods."

  override def grammarName = "invokestatic"
}
