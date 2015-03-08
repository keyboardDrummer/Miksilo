package transformations.bytecode.coreInstructions.objects

import core.particles.{MetaObject, CompilationState}
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object PushNullC extends InstructionC {

  override val key: AnyRef = PushNullKey
  val pushNull = ByteCodeSkeleton.instruction(PushNullC)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("01")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize(instruction: MetaObject): Int = 1

  object PushNullKey

}
