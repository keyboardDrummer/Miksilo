package deltas.bytecode.coreInstructions.objects

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeDelta

object PushNullDelta extends InstructionInstance {

  val pushNull = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("01")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeDelta.intType))

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def grammarName = "aconst_null"
}
