package miksilo.modularLanguages.deltas.javac.expressions.literals

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.javac.expressions.literals.BooleanLiteralDelta.getValue

object BooleanLiteralToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Enables boolean literals to be converted to bytecode"

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(SmallIntegerConstantDelta.integerConstant(if (getValue(literal)) 1 else 0))
  }

  override def shape: NodeShape = BooleanLiteralDelta.Shape

  override def dependencies: Set[Contract] = Set(BooleanLiteralDelta, SmallIntegerConstantDelta)
}
