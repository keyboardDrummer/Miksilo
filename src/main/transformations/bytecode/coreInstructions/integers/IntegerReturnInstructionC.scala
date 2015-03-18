package transformations.bytecode.coreInstructions.integers

import core.particles.node.Node
import core.particles.{CompilationState, Contract}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IntegerReturnInstructionC extends InstructionC {

  override val key: AnyRef = IntegerReturn

  def integerReturn: Node = CodeAttribute.instruction(IntegerReturn)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ac")

  object IntegerReturn

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the integer return instruction, which returns an integer from the current method."
}
