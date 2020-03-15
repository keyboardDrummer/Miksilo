package miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState

object StoreAddressDelta extends InstructionInstance {

  def addressStore(location: Int): Node = CodeAttributeDelta.instruction(shape, Seq(location))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("3a") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("4b") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "StoreAddress")
    InstructionSignature(Seq(stackTop), Seq())
  }

  override def getVariableUpdates(instruction: Node, typeState: ProgramTypeState ): Map[Int, Node] = {
    val variableLocation: Int = CodeAttributeDelta.getInstructionArguments(instruction)(0)
    val _type = typeState.stackTypes.last
    Map(variableLocation -> _type)
  }

  override def grammarName = "astore" //TODO astore_0 etc..
}
