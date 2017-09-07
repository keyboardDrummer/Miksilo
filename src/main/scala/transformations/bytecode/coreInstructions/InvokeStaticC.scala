package transformations.bytecode.coreInstructions

import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute

object InvokeStaticC extends InvokeC {

  override val key: Key = InvokeStaticKey

  def invokeStatic(constantIndex: Any): Node = CodeAttribute.instruction(InvokeStaticKey, Seq(constantIndex))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("b8") ++ shortToBytes(arguments.head)
  }

  object InvokeStaticKey extends Key

  override def description: String = "Defines the invoke static instruction, which can be used to call static methods."
}
