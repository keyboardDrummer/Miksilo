package miksilo.modularLanguages.deltas.javac.expressions.literals

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.PushNullDelta
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta

object NullToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(PushNullDelta.pushNull)
  }

  override def description = "Converts null to bytecode"

  override def shape = NullDelta.Shape

  override def dependencies = Set(NullDelta, PushNullDelta)
}
