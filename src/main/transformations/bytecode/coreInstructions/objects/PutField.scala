package transformations.bytecode.coreInstructions.objects

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.{ByteCodeTypeException, InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.classes.ConstantPool

object PutField extends InstructionC {

  object PutFieldKey
  override val key: AnyRef = PutFieldKey

  def putField(index: Int) = ByteCodeSkeleton.instruction(PutFieldKey, Seq(index))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("b5") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState,
                                          state: TransformationState): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)

    if (stackTop.size != 2)
      throw new ByteCodeTypeException("PutField requires two arguments on the stack.")

    val valueType = stackTop(1)
    val objectType = stackTop(0)

    assertObjectTypeStackTop(objectType, "PutField")

    new InstructionSignature(Seq.empty, Seq(valueType, objectType))
  }
}
