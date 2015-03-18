package transformations.bytecode.coreInstructions.objects

import core.particles.CompilationState
import core.particles.node.MetaObject
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.ClassRefConstant
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.classes.QualifiedClassName
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

  override def getSignature(instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val constantPool = ByteCodeSkeleton.getConstantPool(state)
    val location = CodeAttribute.getInstructionArguments(instruction)(0)
    val classRef = constantPool.getValue(location).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[QualifiedClassName]
    val classType = ObjectTypeC.objectType(className)
    InstructionSignature(Seq.empty, Seq(classType))
  }
}
