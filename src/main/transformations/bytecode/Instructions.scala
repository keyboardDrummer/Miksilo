package transformations.bytecode

import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.additions.LabelledTargets.LabelKey
import transformations.javac.classes.ConstantPool

object Instructions {

  def getInstructionInputTypes(constantPool: ConstantPool, instruction: Node, state: CompilationState): Seq[Node] =
    getInstructionInAndOutputs(constantPool, instruction, state)._1

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: Node, state: CompilationState): (Seq[Node], Seq[Node]) =
    instruction.clazz match {
      case LabelKey => (Seq(), Seq())
    }

  def getInstructionOutputTypes(constantPool: ConstantPool, instruction: Node, state: CompilationState): Seq[Node] =
    getInstructionInAndOutputs(constantPool, instruction, state)._2
}
