package deltas.bytecode.coreInstructions.integers.integerCompare

import core.language.Compilation
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.InstructionInstance

trait JumpInstruction extends InstructionInstance {

  override def getInstructionSize(compilation: Compilation): Int = 3

  override def jumpBehavior: JumpBehavior = JumpBehavior(true, true)
}
