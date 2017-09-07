package transformations.bytecode.coreInstructions.objects

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{ByteCodeTypeException, InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState

object PutField extends InstructionC {

  object PutFieldKey extends Key
  override val key: Key = PutFieldKey

  def putField(index: Any) = CodeAttribute.instruction(PutFieldKey, Seq(index))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("b5") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)

    if (stackTop.size != 2)
      throw new ByteCodeTypeException("PutField requires two arguments on the stack.")

    val valueType = stackTop(1)
    val objectType = stackTop(0)

    assertObjectTypeStackTop(objectType, "PutField")

    new InstructionSignature(Seq.empty, Seq(valueType, objectType))
  }
}
