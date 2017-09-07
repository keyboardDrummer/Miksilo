package transformations.bytecode.coreInstructions.objects

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.{FieldRefConstant, NameAndType}
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object GetFieldC extends InstructionC {

  override val key: Key = GetFieldKey

  def construct(fieldRefIndex: Any): Node = CodeAttribute.instruction(GetFieldKey, Seq(fieldRefIndex))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("b4") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "getField")
    new InstructionSignature(Seq(stackTop), Seq(getReturnType(state.program.constantPool, instruction)))
  }

  def getReturnType(constantPool: ConstantPool, getField: Node): Node = {
    val fieldRefIndex = CodeAttribute.getInstructionArguments(getField).head
    val fieldRef = constantPool.getValue(fieldRefIndex).asInstanceOf[Node]
    val nameAndType = constantPool.getValue(FieldRefConstant.getNameAndTypeIndex(fieldRef)).asInstanceOf[Node]
    val fieldType = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[Node]
    fieldType
  }

  override def getInstructionSize: Int = 3

  object GetFieldKey extends Key
}
