package transformations.bytecode.coreInstructions.integers

import core.particles.{CompilationState, Contract, MetaObject}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object StoreIntegerC extends InstructionC {

  override val key: AnyRef = IntegerStore

  def integerStore(location: Int) = CodeAttribute.instruction(IntegerStore, Seq(location))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("36") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("3b") + location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def getVariableUpdates(instruction: MetaObject, typeState: ProgramTypeState ): Map[Int, MetaObject] =
    Map(CodeAttribute.getInstructionArguments(instruction)(0) -> IntTypeC.intType)

  object IntegerStore

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the integer store instruction, which stores the top stack integer in a variable."
}
