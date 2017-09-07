package transformations.bytecode.coreInstructions.objects

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.{ClassRefConstant, QualifiedClassNameConstant}
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.ObjectTypeC
import transformations.javac.classes.skeleton.QualifiedClassName

object NewByteCodeC extends InstructionC {

  object NewByteCodeKey extends Key
  
  def newInstruction(classRef: Any) = CodeAttribute.instruction(NewByteCodeKey, Seq(classRef))
  
  override val key: Key = NewByteCodeKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)
    PrintByteCode.hexToBytes("bb") ++ PrintByteCode.shortToBytes(location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val constantPool = state.program.constantPool
    val location = CodeAttribute.getInstructionArguments(instruction).head
    val classRef = constantPool.getValue(location).asInstanceOf[Node]
    val className = QualifiedClassNameConstant.get(constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[Node])
    val classType = ObjectTypeC.objectType(className)
    InstructionSignature(Seq.empty, Seq(classType))
  }
}
