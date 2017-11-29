package deltas.bytecode

import core.deltas.Language
import core.deltas.node.Node
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.classes.ConstantPool

object Instructions {

  def getInstructionInputTypes(constantPool: ConstantPool, instruction: Node, state: Language): Seq[Node] =
    getInstructionInAndOutputs(constantPool, instruction, state)._1

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: Node, state: Language): (Seq[Node], Seq[Node]) =
    instruction.clazz match {
      case LabelDelta.LabelKey => (Seq(), Seq())
    }

  def getInstructionOutputTypes(constantPool: ConstantPool, instruction: Node, state: Language): Seq[Node] =
    getInstructionInAndOutputs(constantPool, instruction, state)._2
}
