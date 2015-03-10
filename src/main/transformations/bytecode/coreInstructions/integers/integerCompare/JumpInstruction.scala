package transformations.bytecode.coreInstructions.integers.integerCompare

import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.coreInstructions.InstructionC

trait JumpInstruction extends InstructionC {

  override def getInstructionSize: Int = 3

  override def jumpBehavior: JumpBehavior = new JumpBehavior(true, true)
}
