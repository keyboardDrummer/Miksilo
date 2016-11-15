package transformations.bytecode.coreInstructions.doubles

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.DoubleTypeC

object DoubleReturnInstructionC extends InstructionC {

  override val key: Key = DoubleReturn

  def create: Node = CodeAttribute.instruction(DoubleReturn)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(DoubleTypeC.doubleType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("af")

  object DoubleReturn extends Key

  override def dependencies: Set[Contract] = super.dependencies ++ Set(DoubleTypeC)

  override def description: String = "Defines the double return instruction, which returns a double from the current method."
}
