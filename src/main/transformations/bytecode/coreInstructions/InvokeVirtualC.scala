package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object InvokeVirtualC extends InvokeC {

  override val key: AnyRef = InvokeVirtual

  def invokeVirtual(methodRefIndex: Int) = ByteCodeSkeleton.instruction(InvokeVirtual, Seq(methodRefIndex))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("b6") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState,
                                          state: TransformationState): InstructionSignature = {
    getInstanceInstructionInAndOutputs(constantPool, instruction, typeState, state)
  }

  object InvokeVirtual

}
