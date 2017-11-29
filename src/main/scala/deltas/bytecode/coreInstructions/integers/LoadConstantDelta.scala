package deltas.bytecode.coreInstructions.integers

import core.deltas.{Compilation, Language}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.IntegerInfoConstant
import deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object LoadConstantDelta extends InstructionDelta
{
  object LoadConstantKey extends NodeClass
  object IntegerConstantIndex extends NodeField
  override val key = LoadConstantKey

  def integerConstant(value: Any) = LoadConstantKey.create(IntegerConstantIndex -> value)

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
