package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.{FieldRefConstant, NameAndType}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object GetStaticC extends InstructionC {

  override val key: Key = GetStaticKey

  def getStatic(fieldRefIndex: Int): Node = CodeAttribute.instruction(GetStaticKey, Seq(fieldRefIndex))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("b2") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    new InstructionSignature(Seq(), Seq(getReturnType(state.program.constantPool, instruction)))

  def getReturnType(constantPool: ConstantPool, getStatic: Node): Node = {
    val location = CodeAttribute.getInstructionArguments(getStatic).head
    val fieldRef = constantPool.getValue(location).asInstanceOf[Node]
    val nameAndType = constantPool.getValue(FieldRefConstant.getNameAndTypeIndex(fieldRef)).asInstanceOf[Node]
    val fieldType = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[Node]
    fieldType
  }

  override def getInstructionSize: Int = 3

  object GetStaticKey extends Key

  override def description: String = "Defines the getStatic instruction, which retrieves a value from a static field."
}
