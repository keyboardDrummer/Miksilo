package deltas.bytecode.coreInstructions.integers.integerCompare

import deltas.bytecode.attributes.CodeAttribute.JumpBehavior
import deltas.bytecode.coreInstructions.InstructionDelta

trait JumpInstruction extends InstructionDelta {

  override def getInstructionSize: Int = 3

  override def jumpBehavior: JumpBehavior = new JumpBehavior(true, true)
}
