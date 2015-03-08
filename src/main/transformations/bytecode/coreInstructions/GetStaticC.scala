package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.{FieldRefConstant, NameAndType}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool



object GetStaticC extends InstructionC {

  override val key: AnyRef = GetStaticKey

  def getStatic(fieldRefIndex: Int): MetaObject = instruction(GetStaticKey, Seq(fieldRefIndex))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("b2") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    new InstructionSignature(Seq(), Seq(getReturnType(constantPool, instruction)))

  def getReturnType(constantPool: ConstantPool, getStatic: MetaObject): MetaObject = {
    val location = ByteCodeSkeleton.getInstructionArguments(getStatic)(0)
    val fieldRef = constantPool.getValue(location).asInstanceOf[MetaObject]
    val nameAndType = constantPool.getValue(FieldRefConstant.getNameAndTypeIndex(fieldRef)).asInstanceOf[MetaObject]
    val fieldType = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[MetaObject]
    fieldType
  }

  override def getInstructionSize(instruction: MetaObject): Int = 3

  object GetStaticKey

  override def description: String = "Defines the getStatic instruction, which retrieves a value from a static field."
}
