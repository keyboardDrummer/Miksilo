package deltas.javac.expressions.literals

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.javac.expressions.ConvertsToByteCode
import deltas.javac.expressions.literals.BooleanLiteralDelta.getValue

object BooleanLiteralToByteCodeDelta extends ConvertsToByteCode {

  override def description: String = "Enables boolean literals to be converted to bytecode"

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(SmallIntegerConstantDelta.integerConstant(if (getValue(literal)) 1 else 0))
  }

  override def shape: NodeShape = BooleanLiteralDelta.Shape

  override def dependencies: Set[Contract] = Set(BooleanLiteralDelta, SmallIntegerConstantDelta)
}
