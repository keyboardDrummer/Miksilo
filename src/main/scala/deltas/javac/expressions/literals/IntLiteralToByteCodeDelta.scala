package deltas.javac.expressions.literals

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.bytecode.constants.IntegerInfoConstant
import deltas.bytecode.coreInstructions.integers.{LoadConstantDelta, SmallIntegerConstantDelta}
import deltas.expression.IntLiteralDelta
import deltas.javac.expressions.ConvertsToByteCode
import deltas.expression.IntLiteralDelta.{Value, getValue}

object IntLiteralToByteCodeDelta extends ConvertsToByteCode {

  override def description: String = "Enabled int literals to convert to bytecode"

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    val value: Int = getValue(literal)
    if (-1 <= value && value <= 5) {
      val node = literal.current.shallowClone
      node.data.remove(Value)
      node.replaceWith(SmallIntegerConstantDelta.integerConstant(value), keepData = true)
      Seq(node) //TODO dit mooier maken. Maak de nieuwe node gewoon en en schuif deze over de oude node.
    }
    else
    {
      Seq(LoadConstantDelta.integerConstant(IntegerInfoConstant.construct(value)))
    }
  }


  override def dependencies: Set[Contract] = Set(IntLiteralDelta, SmallIntegerConstantDelta, LoadConstantDelta)

  override def shape: NodeShape = IntLiteralDelta.Shape
}
