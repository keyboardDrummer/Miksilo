package transformations.bytecode.coreInstructions.longs

import core.particles.{MetaObject, CompilationState}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.{IntTypeC, LongTypeC}

object CompareLongC extends InstructionC {

  val compareLong = new MetaObject(CompareLongKey)

  object CompareLongKey

  override val key: AnyRef = CompareLongKey

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType, LongTypeC.longType), Seq(IntTypeC.intType))
}
