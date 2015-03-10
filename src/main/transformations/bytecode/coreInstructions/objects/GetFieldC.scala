package transformations.bytecode.coreInstructions.objects

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.{FieldRefConstant, NameAndType}
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object GetFieldC extends InstructionC {

  override val key: AnyRef = GetFieldKey

  def construct(fieldRefIndex: Int): MetaObject = CodeAttribute.instruction(GetFieldKey, Seq(fieldRefIndex))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("b4") ++ shortToBytes(arguments(0))
  }

  override def getSignature(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "getField")
    new InstructionSignature(Seq(stackTop), Seq(getReturnType(constantPool, instruction)))
  }

  def getReturnType(constantPool: ConstantPool, getField: MetaObject): MetaObject = {
    val fieldRefIndex = CodeAttribute.getInstructionArguments(getField)(0)
    val fieldRef = constantPool.getValue(fieldRefIndex).asInstanceOf[MetaObject]
    val nameAndType = constantPool.getValue(FieldRefConstant.getNameAndTypeIndex(fieldRef)).asInstanceOf[MetaObject]
    val fieldType = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[MetaObject]
    fieldType
  }

  override def getInstructionSize(): Int = 3

  object GetFieldKey
}
