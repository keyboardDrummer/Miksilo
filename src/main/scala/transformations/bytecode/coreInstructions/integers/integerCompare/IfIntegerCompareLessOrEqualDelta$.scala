package transformations.bytecode.coreInstructions.integers.integerCompare

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object IfIntegerCompareLessOrEqualDelta$ extends JumpInstruction { //TODO superclasse maken om wat van deze jump instructies onder te schuiven

  override val key: Key = IfIntegerCompareLessEqualKey

  def create(target: Int): Node = CodeAttribute.instruction(key, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("a4") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object IfIntegerCompareLessEqualKey extends Key
}
