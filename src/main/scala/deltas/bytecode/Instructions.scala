package deltas.bytecode

import core.particles.Language
import core.particles.node.Node
import deltas.bytecode.additions.LabelledLocations
import deltas.bytecode.additions.LabelledLocations.LabelKey
import deltas.javac.classes.ConstantPool

object Instructions {

  def getInstructionInputTypes(constantPool: ConstantPool, instruction: Node, state: Language): Seq[Node] =
    getInstructionInAndOutputs(constantPool, instruction, state)._1

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: Node, state: Language): (Seq[Node], Seq[Node]) =
    instruction.clazz match {
      case LabelKey => (Seq(), Seq())
    }

  def getInstructionOutputTypes(constantPool: ConstantPool, instruction: Node, state: Language): Seq[Node] =
    getInstructionInAndOutputs(constantPool, instruction, state)._2
}
