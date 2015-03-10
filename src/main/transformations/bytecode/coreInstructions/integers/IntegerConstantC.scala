package transformations.bytecode.coreInstructions.integers

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IntegerConstantC extends InstructionC {

  override val key: AnyRef = IntegerConstantKey

  def integerConstant(value: Int) = {
    require (value <= 5)
    require (value >= -1)
    CodeAttribute.instruction(IntegerConstantKey, Seq(value))
  }

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    byteToBytes(3 + CodeAttribute.getInstructionArguments(instruction)(0))
  }

  override def getSignature(instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize(): Int = 1

  private object IntegerConstantKey

  override def description: String = "Defines the integer constant instruction, which places an integer between -1 and 5 on the stack."
}
