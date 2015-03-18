package transformations.bytecode.coreInstructions.integers.integerCompare

import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IfIntegerCompareLessC extends JumpInstruction {

  override val key: AnyRef = IfIntegerCompareLessKey

  def ifIntegerCompareGreater(target: Int): Node = CodeAttribute.instruction(IfIntegerCompareLessKey, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("a1") ++ shortToBytes(arguments(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object IfIntegerCompareLessKey

}
