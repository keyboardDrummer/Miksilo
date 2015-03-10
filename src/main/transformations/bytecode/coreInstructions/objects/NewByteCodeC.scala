package transformations.bytecode.coreInstructions.objects

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.ClassRefConstant
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.{ConstantPool, QualifiedClassName}
import transformations.types.ObjectTypeC

object NewByteCodeC extends InstructionC {

  object NewByteCodeKey
  
  def newInstruction(classRefIndex: Int) = CodeAttribute.instruction(NewByteCodeKey, Seq(classRefIndex))
  
  override val key: AnyRef = NewByteCodeKey

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)
    PrintByteCode.hexToBytes("bb") ++ PrintByteCode.shortToBytes(location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val location = CodeAttribute.getInstructionArguments(instruction)(0)
    val classRef = constantPool.getValue(location).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[QualifiedClassName]
    val classType = ObjectTypeC.objectType(className)
    InstructionSignature(Seq.empty, Seq(classType))
  }
}
