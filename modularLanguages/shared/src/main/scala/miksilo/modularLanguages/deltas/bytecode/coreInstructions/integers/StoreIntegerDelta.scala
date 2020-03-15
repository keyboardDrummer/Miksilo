package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.core.deltas.Contract
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object StoreIntegerDelta extends InstructionInstance {

  def integerStore(location: Int) = CodeAttributeDelta.instruction(shape, Seq(location))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("36") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("3b") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeDelta.intType), Seq())

  override def getVariableUpdates(instruction: Node, typeState: ProgramTypeState ): Map[Int, Node] =
    Map(CodeAttributeDelta.getInstructionArguments(instruction)(0) -> IntTypeDelta.intType)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeDelta)

  override def description: String = "Defines the integer store instruction, which stores the top stack integer in a variable."

  override def grammarName = "istore" //TODO missing istore_0 etc..
}
