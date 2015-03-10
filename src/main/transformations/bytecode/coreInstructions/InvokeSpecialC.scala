package transformations.bytecode.coreInstructions

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object InvokeSpecialC extends InvokeC {
  override val key: AnyRef = InvokeSpecialKey

  def invokeSpecial(location: Int): MetaObject = CodeAttribute.instruction(InvokeSpecialKey, Seq(location))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("b7") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    getInstanceInstructionInAndOutputs(constantPool, instruction, typeState, state)
  }

  object InvokeSpecialKey

  override def description: String = "Defines the invoke special method, which can be used to call constructors."
}