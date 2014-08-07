package transformations.bytecode.coreInstructions.integerCompare

import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.coreInstructions.InstructionC

trait JumpInstruction extends InstructionC {

  override def getInstructionSize: Int = 3

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(true, true)
}
