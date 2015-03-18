package transformations.bytecode.coreInstructions.integers.integerCompare

import core.particles.CompilationState
import core.particles.node.MetaObject
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IfZeroC extends JumpInstruction {
  override val key: AnyRef = IfZeroKey

  def ifZero(target: Int) = CodeAttribute.instruction(IfZeroKey, Seq(target))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("99") ++ shortToBytes(arguments(0))
  }

  override def getSignature(instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(IntTypeC.intType), Seq())

  object IfZeroKey

}
