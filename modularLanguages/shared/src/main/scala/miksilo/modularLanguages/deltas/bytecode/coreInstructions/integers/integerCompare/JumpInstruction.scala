package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare

import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InstructionInstance

trait JumpInstruction extends InstructionInstance {

  override def getInstructionSize(compilation: Compilation): Int = 3

  override def jumpBehavior: JumpBehavior = JumpBehavior(true, true)
}
