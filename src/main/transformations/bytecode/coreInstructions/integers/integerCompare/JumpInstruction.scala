package transformations.bytecode.coreInstructions.integers.integerCompare

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.coreInstructions.InstructionC

trait JumpInstruction extends InstructionC {

  override def getInstructionSize(instruction: MetaObject): Int = 3

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(true, true)
}
