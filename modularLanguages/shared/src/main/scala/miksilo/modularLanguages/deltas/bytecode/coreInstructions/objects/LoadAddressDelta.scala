package miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects

import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState

object LoadAddressDelta extends InstructionInstance {

  def addressLoad(location: Int): Node = CodeAttributeDelta.instruction(shape, Seq(location))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      PrintByteCode.hexToBytes("19") ++ PrintByteCode.byteToBytes(location)
    else
      PrintByteCode.byteToBytes(PrintByteCode.hexToInt("2a") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)

    InstructionSignature(Seq(), Seq(typeState.variableTypes(location)))
  }

  override def grammarName = "aload" //TODO aload_0 etc..
}
