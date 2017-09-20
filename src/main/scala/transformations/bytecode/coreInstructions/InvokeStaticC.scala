package transformations.bytecode.coreInstructions

import core.particles.node.{Key, Node, NodeClass}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute

object InvokeStaticC extends InvokeC {

  override val key: Key = InvokeStaticKey

  def invokeStatic(constantIndex: Any): Node = InvokeStaticKey.create(MethodRef -> constantIndex)

  override def getInstructionSize: Int = 3
  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    hexToBytes("b8") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  object InvokeStaticKey extends NodeClass

  override def description: String = "Defines the invoke static instruction, which can be used to call static methods."
}
