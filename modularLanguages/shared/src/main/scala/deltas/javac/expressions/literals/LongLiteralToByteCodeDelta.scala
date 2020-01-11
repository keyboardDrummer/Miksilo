package deltas.javac.expressions.literals

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.longs.PushLongDelta
import deltas.javac.expressions.ConvertsToByteCodeDelta
import deltas.javac.expressions.literals.LongLiteralDelta.getValue

object LongLiteralToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(PushLongDelta.constant(getValue(literal).toInt))
  }

  override def description = "Converts long literals to bytecode"

  override def shape = LongLiteralDelta.Shape

  override def dependencies = Set(LongLiteralDelta, PushLongDelta)
}
