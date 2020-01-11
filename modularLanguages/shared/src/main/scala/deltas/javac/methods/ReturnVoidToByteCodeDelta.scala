package deltas.javac.methods

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.VoidReturnInstructionDelta
import deltas.javac.expressions.ConvertsToByteCodeDelta

object ReturnVoidToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(_return: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(VoidReturnInstructionDelta.voidReturn)
  }

  override def shape = ReturnVoidDelta.Shape

  override def description = "Transform return without expression to bytecode"

  override def dependencies = Set(VoidReturnInstructionDelta)
}
