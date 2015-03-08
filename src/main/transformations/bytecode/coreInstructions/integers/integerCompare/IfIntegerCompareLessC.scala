package transformations.bytecode.coreInstructions.integers.integerCompare

import core.transformation.{CompilationState, MetaObject}
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton._
import PrintByteCode._
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IfIntegerCompareLessC extends JumpInstruction {

  override val key: AnyRef = IfIntegerCompareLessKey

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction(IfIntegerCompareLessKey, Seq(target))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("a1") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object IfIntegerCompareLessKey

}
