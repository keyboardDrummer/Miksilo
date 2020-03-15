package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._

object InvokeStaticDelta extends InvokeDelta {

  def invokeStatic(constantIndex: Any): Node = shape.create(MethodRef -> constantIndex)

  override def getInstructionSize(compilation: Compilation): Int = 3
  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    hexToBytes("b8") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  override def description: String = "Defines the invoke static instruction, which can be used to call static methods."

  override def grammarName = "invokestatic"
}
