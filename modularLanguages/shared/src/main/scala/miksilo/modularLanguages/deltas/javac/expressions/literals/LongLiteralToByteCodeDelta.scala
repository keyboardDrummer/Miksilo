package miksilo.modularLanguages.deltas.javac.expressions.literals

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs.PushLongDelta
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.javac.expressions.literals.LongLiteralDelta.getValue

object LongLiteralToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(PushLongDelta.constant(getValue(literal).toInt))
  }

  override def description = "Converts long literals to bytecode"

  override def shape = LongLiteralDelta.Shape

  override def dependencies = Set(LongLiteralDelta, PushLongDelta)
}
