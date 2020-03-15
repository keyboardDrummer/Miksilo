package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState

object InvokeVirtualDelta extends InvokeDelta {

  def invokeVirtual(methodRefIndex: Any) = shape.create(MethodRef -> methodRefIndex)

  override def getInstructionSize(compilation: Compilation): Int = 3
  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    hexToBytes("b6") ++ shortToBytes(instruction(MethodRef).asInstanceOf[Int])
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    getInstanceInstructionSignature(instruction, typeState, language)
  }

  override def description: String = "Defines the invoke virtual instruction, which can be used to call virtual methods."

  override def grammarName = "invokevirtual"
}
