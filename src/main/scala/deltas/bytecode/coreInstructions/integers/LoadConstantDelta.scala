package deltas.bytecode.coreInstructions.integers

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField}
import core.language.Language
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.IntegerInfoConstant
import deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object LoadConstantDelta extends InstructionDelta
{
  object IntegerConstantIndex extends NodeField

  def integerConstant(value: Any) = key.create(IntegerConstantIndex -> value)

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    InstructionSignature(Seq.empty, Seq(IntTypeC.intType))
  }

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val index: Int = instruction(IntegerConstantIndex).asInstanceOf[Int]
    hexToBytes("12") ++ byteToBytes(index)
  }
  override def getInstructionSize: Int = 2

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(IntegerConstantIndex -> IntegerInfoConstant.key))
  }

  override def argumentsGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(IntegerConstantIndex)
  }

  override def grammarName = "ldc"
}
