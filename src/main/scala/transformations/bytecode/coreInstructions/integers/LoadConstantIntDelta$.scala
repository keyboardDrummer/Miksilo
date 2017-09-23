package transformations.bytecode.coreInstructions.integers

import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.IntegerInfoConstant
import transformations.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object LoadConstantIntDelta$ extends InstructionDelta
{
  object LoadConstantKey extends NodeClass
  object IntegerConstantIndex extends NodeField
  override val key = LoadConstantKey

  def integerConstant(value: Any) = LoadConstantKey.create(IntegerConstantIndex -> value)

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    InstructionSignature(Seq.empty, Seq(IntTypeC.intType))
  }

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val index: Int = instruction(IntegerConstantIndex).asInstanceOf[Int]
    hexToBytes("12") ++ byteToBytes(index)
  }
  override def getInstructionSize: Int = 2

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(IntegerConstantIndex -> IntegerInfoConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue) = grammars.find(ConstantPoolIndexGrammar).as(IntegerConstantIndex)
}
