package deltas.bytecode.coreInstructions.integers

import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField}
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.IntegerInfoConstant
import deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeDelta

object LoadConstantDelta extends InstructionInstance
{
  object IntegerConstantIndex extends NodeField

  def integerConstant(value: Any) = shape.create(IntegerConstantIndex -> value)

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    InstructionSignature(Seq.empty, Seq(IntTypeDelta.intType))
  }

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val index: Int = instruction(IntegerConstantIndex).asInstanceOf[Int]
    hexToBytes("12") ++ byteToBytes(index)
  }
  override def getInstructionSize(compilation: Compilation): Int = 2

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(IntegerConstantIndex -> IntegerInfoConstant.shape))
  }

  override def argumentsGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(IntegerConstantIndex)
  }

  override def grammarName = "ldc"
}
