package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.VoidReturnInstructionDelta
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta

object ReturnVoidToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(_return: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(VoidReturnInstructionDelta.voidReturn)
  }

  override def shape = ReturnVoidDelta.Shape

  override def description = "Transform return without expression to bytecode"

  override def dependencies = Set(VoidReturnInstructionDelta)
}
