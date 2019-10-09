package deltas.javac.expressions.literals

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.objects.PushNullDelta
import deltas.javac.expressions.ConvertsToByteCodeDelta

object NullToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(PushNullDelta.pushNull)
  }

  override def description = "Converts null to bytecode"

  override def shape = NullDelta.Shape

  override def dependencies = Set(NullDelta, PushNullDelta)
}
